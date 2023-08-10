open! StdLabels
open! Javascript_api

let little_endian =
  let uint16_array = Typed_array.create_uint16_length 1 in
  Typed_array.set uint16_array (`List [ 0x1122 ]) ();
  let bytes = Data_view.create (Typed_array.buffer uint16_array) in
  match Data_view.get_uint8 bytes ~byte_offset:0 with
  | 0x11 -> false
  | 0x22 -> true
  | _ -> assert false
;;

module Address : sig
  type t = private int [@@immediate]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val add : t -> int -> t
  val zero : t
  val of_int : int -> t
  val to_int : t -> int
end = struct
  type t = int

  let t_to_js = Ojs.int_to_js
  let t_of_js = Ojs.int_of_js
  let add = ( + )
  let zero = 0
  let of_int t = t
  let to_int t = t
end

module Module =
[%js:
type t

val t_to_js : t -> Ojs.t
val create : unit -> t Promise.Any_error.t [@@js.global "createModule"]
val heap8 : t -> int Typed_array.t [@@js.get "HEAP8"]
val malloc : t -> int -> Address.t [@@js.call "_malloc"]
val string_to_new_utf8 : t -> string -> Address.t [@@js.call "stringToNewUTF8"]
val utf8_to_string : t -> Address.t -> string [@@js.call "UTF8ToString"]
val free : t -> Address.t -> unit [@@js.call "_free"]
val set_canvas : t -> Element.t -> unit [@@js.set]]

let (ready : Module.t Promise.Any_error.t), instance =
  let instance_ref = ref None in
  let instance_promise = Module.create () in
  let ready =
    Promise.map instance_promise ~f:(fun instance ->
      instance_ref := Some instance;
      instance)
  in
  let instance =
    lazy
      (match !instance_ref with
       | None ->
         failwith
           "Cannot use the emscripten API until it is marked ready, please wait on the \
            ready promise."
       | Some instance -> instance)
  in
  ready, instance
;;

let heap =
  Lazy.map
    (fun instance -> Module.heap8 instance |> Typed_array.buffer |> Data_view.create)
    instance
;;

let memcpy_dataview ~(dst : Address.t) ~src =
  let heap8 = Lazy.force instance |> Module.heap8 in
  let src =
    Typed_array.create_uint8_buffer
      (Data_view.buffer src)
      ~length:(Data_view.byte_length src)
      ~byte_offset:(Data_view.byte_offset src)
      ()
  in
  Typed_array.set heap8 (`Typed_array src) ~target_offset:(dst :> int) ()
;;

module Primitive = struct
  type 'a t =
    | Int8 : int t
    | Int16 : int t
    | Int32 : int t
    | UInt8 : int t
    | UInt16 : int t
    | UInt32 : int t
    | F32 : float t
    | F64 : float t
    | Lift : 'a t * ('a -> 'b) * ('b -> 'a) -> 'b t

  let rec to_ojs : type a. a t -> a -> Ojs.t =
   fun t v ->
    match t with
    | Int8 -> Ojs.int_to_js v
    | Int16 -> Ojs.int_to_js v
    | Int32 -> Ojs.int_to_js v
    | UInt8 -> Ojs.int_to_js v
    | UInt16 -> Ojs.int_to_js v
    | UInt32 -> Ojs.int_to_js v
    | F32 -> Ojs.float_to_js v
    | F64 -> Ojs.float_to_js v
    | Lift (t, _, f) -> to_ojs t (f v)
 ;;

  let rec of_ojs : type a. a t -> Ojs.t -> a =
   fun t v ->
    match t with
    | Int8 -> Ojs.int_of_js v
    | Int16 -> Ojs.int_of_js v
    | Int32 -> Ojs.int_of_js v
    | UInt8 -> Ojs.int_of_js v
    | UInt16 -> Ojs.int_of_js v
    | UInt32 -> Ojs.int_of_js v
    | F32 -> Ojs.float_of_js v
    | F64 -> Ojs.float_of_js v
    | Lift (t, f, _) -> of_ojs t v |> f
 ;;

  let char = Lift (Int8, Char.chr, Char.code)
end

module Memory_representation = struct
  module Setter_getter = struct
    type 'a t =
      { set : Address.t -> 'a -> unit
      ; get : Address.t -> 'a
      }

    let base ~set ~get = { set; get }

    let lift { set; get } ~map ~contramap =
      { set = (fun pointer v -> set pointer (map v))
      ; get = (fun pointer -> get pointer |> contramap)
      }
    ;;

    let shift { set; get } amount =
      { set = (fun pointer v -> set (Address.add pointer amount) v)
      ; get = (fun pointer -> get (Address.add pointer amount))
      }
    ;;
  end

  type 'a t =
    { setter_getter : 'a Setter_getter.t
    ; size : int
    ; alignment : int
    }

  let lift { setter_getter; size; alignment } ~map ~contramap =
    { setter_getter = Setter_getter.lift setter_getter ~map ~contramap; size; alignment }
  ;;

  let int8_t =
    { setter_getter =
        Setter_getter.base
          ~set:(fun address value ->
            Data_view.set_int8
              (Lazy.force heap)
              ~byte_offset:(Address.to_int address)
              ~value)
          ~get:(fun address ->
            Data_view.get_int8 (Lazy.force heap) ~byte_offset:(Address.to_int address))
    ; size = 1
    ; alignment = 1
    }
  ;;

  let uint8_t =
    { setter_getter =
        Setter_getter.base
          ~set:(fun address value ->
            Data_view.set_uint8
              (Lazy.force heap)
              ~byte_offset:(Address.to_int address)
              ~value)
          ~get:(fun address ->
            Data_view.get_uint8 (Lazy.force heap) ~byte_offset:(Address.to_int address))
    ; size = 1
    ; alignment = 1
    }
  ;;

  let[@inline always] setter f address value =
    f (Lazy.force heap) ~byte_offset:(Address.to_int address) ~value ~little_endian
  ;;

  let[@inline always] getter f address =
    f (Lazy.force heap) ~byte_offset:(Address.to_int address) ~little_endian
  ;;

  let int16_t =
    { setter_getter =
        Setter_getter.base
          ~set:(setter Data_view.set_int16)
          ~get:(getter Data_view.get_int16)
    ; size = 2
    ; alignment = 2
    }
  ;;

  let int32_t =
    { setter_getter =
        Setter_getter.base
          ~set:(setter Data_view.set_int32)
          ~get:(getter Data_view.get_int32)
    ; size = 4
    ; alignment = 4
    }
  ;;

  let uint16_t =
    { setter_getter =
        Setter_getter.base
          ~set:(setter Data_view.set_uint16)
          ~get:(getter Data_view.get_uint16)
    ; size = 2
    ; alignment = 2
    }
  ;;

  let uint32_t =
    { setter_getter =
        Setter_getter.base
          ~set:(setter Data_view.set_uint32)
          ~get:(getter Data_view.get_uint32)
    ; size = 4
    ; alignment = 4
    }
  ;;

  let float32_t =
    { setter_getter =
        Setter_getter.base
          ~set:(setter Data_view.set_float32)
          ~get:(getter Data_view.get_float32)
    ; size = 4
    ; alignment = 4
    }
  ;;

  let float64_t =
    { setter_getter =
        Setter_getter.base
          ~set:(setter Data_view.set_float64)
          ~get:(getter Data_view.get_float64)
    ; size = 8
    ; alignment = 8
    }
  ;;

  let char = lift uint8_t ~map:Char.code ~contramap:Char.chr
  let address = lift uint32_t ~map:Address.to_int ~contramap:Address.of_int

  module Structure = struct
    let empty_struct =
      { setter_getter = { set = (fun _pointer () -> ()); get = (fun _pointer -> ()) }
      ; size = 0
      ; alignment = 1
      }
    ;;

    let field t (v : _ t) =
      let setter_getter = Setter_getter.shift v.setter_getter t.size in
      let t =
        { setter_getter =
            { set =
                (fun address (rest, x) ->
                  setter_getter.set address x;
                  t.setter_getter.set address rest)
            ; get =
                (fun address -> t.setter_getter.get address, setter_getter.get address)
            }
        ; size = t.size + v.size
        ; alignment = Int.max t.alignment t.size (* TODO: fix *)
        }
      in
      setter_getter, t
    ;;
  end

  module type Enum_s = sig
    type 'a repr := 'a t
    type t [@@immediate]
    type ocaml_t

    val repr_t : t repr
    val primitive_t : t Primitive.t
    val of_ocaml_t : ocaml_t -> t
    val to_ocaml_t : t -> ocaml_t

    module Orable : sig
      type pure := t
      type t [@@immediate]

      val make : pure -> t
      val repr_t : t repr
      val ( ++ ) : t -> t -> t
      val primitive_t : t Primitive.t
      val zero : t
      val of_ocaml_list : ocaml_t list -> t
    end
  end

  let make_enum (type t) all ~int_value : (module Enum_s with type ocaml_t = t) =
    let of_int = List.map ~f:(fun t -> int_value t, t) all in
    (module struct
      type ocaml_t = t
      type t = int

      let repr_t = int32_t
      let primitive_t = Primitive.Int32
      let of_ocaml_t = int_value
      let to_ocaml_t t = List.assoc t of_int

      module Orable = struct
        type nonrec t = t

        let make t = t
        let repr_t = repr_t
        let primitive_t = primitive_t
        let ( ++ ) = ( lor )
        let zero = 0
        let of_ocaml_list = List.fold_left ~f:(fun t v -> t ++ of_ocaml_t v) ~init:zero
      end
    end)
  ;;
end

module Pointer : sig
  type !'a t

  val unsafe_cast : 'a t -> 'b Memory_representation.t -> 'b t
  val unsafe_of_raw : Address.t -> 'a Memory_representation.t -> 'a t
  val repr_t : 'a Memory_representation.t -> 'a t Memory_representation.t
  val set_indexed : 'a t -> 'a -> int -> unit
  val set : 'a t -> 'a -> unit
  val get_indexed : 'a t -> int -> 'a
  val get : 'a t -> 'a
  val address : _ t -> Address.t
  val free : 'a t -> unit
  val malloc : ?gc:bool -> 'a Memory_representation.t -> int -> 'a t
  val malloc_value : ?gc:bool -> 'a Memory_representation.t -> 'a -> 'a t
end = struct
  type 'a t =
    { address : Address.t
    ; memory_representation : 'a Memory_representation.t
    }

  let address t = t.address
  let unsafe_cast t memory_representation = { t with memory_representation }
  let unsafe_of_raw address memory_representation = { address; memory_representation }
  let free t = Module.free (Lazy.force instance) t.address

  let repr_t memory_representation =
    Memory_representation.lift
      Memory_representation.address
      ~map:(fun t -> t.address)
      ~contramap:(fun address -> { memory_representation; address })
  ;;

  let malloc ?(gc = true) (memory_representation : _ Memory_representation.t) n =
    let t =
      { address = Module.malloc (Lazy.force instance) (memory_representation.size * n)
      ; memory_representation
      }
    in
    if gc then Gc.finalise free t;
    t
  ;;

  let get_indexed { address; memory_representation } n =
    (* TODO fix alignment *)
    memory_representation.setter_getter.get
      (Address.add address (memory_representation.size * n))
  ;;

  let get t = get_indexed t 0

  let set_indexed { address; memory_representation } v n =
    (* TODO fix alignment *)
    memory_representation.setter_getter.set
      (Address.add address (memory_representation.size * n))
      v
  ;;

  let set t v = set_indexed t v 0

  let malloc_value ?gc repr v =
    let t = malloc ?gc repr 1 in
    set t v;
    t
  ;;
end

let ( .@() ) t (get : _ Memory_representation.Setter_getter.t) =
  get.get (Pointer.address t)
;;

let ( .@()<- ) t (get : _ Memory_representation.Setter_getter.t) v =
  get.set (Pointer.address t) v
;;

module Type = struct
  type 'a t =
    | Primitive : 'a Primitive.t -> 'a t
    | Pointer : 'a Memory_representation.t -> 'a Pointer.t t
    | Value : 'a Memory_representation.t -> 'a Pointer.t t
    | Void : unit t

  let to_ojs_fun (type a) (t : a t) =
    match t with
    | Primitive primitive -> Some (Primitive.to_ojs primitive)
    | Pointer _ -> Some (fun v -> Pointer.address v |> Address.t_to_js)
    | Value _ -> Some (fun v -> Pointer.address v |> Address.t_to_js)
    | Void -> None
  ;;
end

module Function = struct
  type ('a, 'ret) t =
    | Returning : 'a Type.t -> ('a, 'a) t
    | Abstract : 'a Type.t * ('b, 'r) t -> ('a -> 'b, 'r) t

  let returning t = Returning t
  let ( @-> ) t rest = Abstract (t, rest)

  let rec return_type : type a ret. (a, ret) t -> ret Type.t = function
    | Returning t -> t
    | Abstract (_, rest) -> return_type rest
  ;;

  let rec arg_count : type a ret. (a, ret) t -> int = function
    | Returning _ -> 0
    | Abstract (Void, rest) -> arg_count rest
    | Abstract ((Primitive _ | Pointer _ | Value _), rest) -> 1 + arg_count rest
  ;;

  let rec build_arguments
    : type a ret.
      (a, ret) t -> arg_index:int -> name:string -> this:Ojs.t -> args:Ojs.t array -> a
    =
   fun t ~arg_index ~name ->
    match t with
    | Returning type_ ->
      (match type_ with
       | Void -> fun ~this ~args -> Ojs.call this name args |> ignore
       | Primitive t ->
         let of_ojs = Primitive.of_ojs t in
         fun ~this ~args -> Ojs.call this name args |> of_ojs
       | Pointer memory_representation ->
         fun ~this ~args ->
           (Ojs.call this name args |> Address.t_of_js |> Pointer.unsafe_of_raw)
             memory_representation
       | Value memory_representation ->
         fun ~this ~args ->
           let pointer = Pointer.malloc memory_representation 1 in
           args.(0) <- Pointer.address pointer |> Address.t_to_js;
           Ojs.call this name args |> ignore;
           pointer)
    | Abstract (arg, rest) ->
      (match arg, Type.to_ojs_fun arg with
       | (Primitive _ | Pointer _ | Value _), Some f ->
         let rest = build_arguments rest ~arg_index:(arg_index + 1) ~name in
         fun ~this ~args v ->
           args.(arg_index) <- f v;
           rest ~this ~args
       | (Primitive _ | Pointer _ | Value _), None -> assert false
       | Void, _ ->
         let rest = build_arguments rest ~arg_index ~name in
         fun ~this ~args () -> rest ~this ~args)
 ;;

  (* Can't curry, maybe a list is needed? *)
  let extern (type a b ret) name (f : (a -> b, ret) t) =
    let arg_count = arg_count f in
    let return_type = return_type f in
    match return_type with
    | Value _ ->
      fun first_arg ->
        let this = Lazy.force instance |> Module.t_to_js in
        let args = Array.make (arg_count + 1) Ojs.null in
        build_arguments f ~arg_index:1 ~name ~args ~this first_arg
    | _ ->
      fun first_arg ->
        let this = Lazy.force instance |> Module.t_to_js in
        let args = Array.make arg_count Ojs.null in
        build_arguments f ~arg_index:0 ~name first_arg ~this ~args
  ;;
end

module C_string = struct
  type t = char Pointer.t

  let t : t Type.t = Pointer Memory_representation.char

  let of_string ?(gc = true) string =
    let address = Module.string_to_new_utf8 (Lazy.force instance) string in
    let pointer = Pointer.unsafe_of_raw address Memory_representation.char in
    if gc then Gc.finalise Pointer.free pointer;
    pointer
  ;;

  let of_data_view ?gc (data_view : Data_view.t) =
    let t =
      Pointer.malloc ?gc Memory_representation.char (Data_view.byte_length data_view)
    in
    memcpy_dataview ~dst:(Pointer.address t) ~src:data_view;
    t
  ;;

  let to_string (t : t) = Module.utf8_to_string (Lazy.force instance) (Pointer.address t)
end
