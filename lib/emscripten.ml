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

module Module = struct
  include
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

  let malloc t size = malloc t size
  let free t size = free t size
  let string_to_new_utf8 t size = string_to_new_utf8 t size
end

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

  let char = Lift (UInt8, Char.chr, Char.code)
  let bool = Lift (UInt8, ( = ) 0, Bool.to_int)
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
  let bool = lift uint8_t ~map:Bool.to_int ~contramap:(( = ) 0)
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
    val ocaml_primitive_t : ocaml_t Primitive.t
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
      let ocaml_primitive_t = Primitive.Lift (primitive_t, to_ocaml_t, of_ocaml_t)

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
  val malloc_lazy : ?gc:bool -> 'a Memory_representation.t -> int -> 'a t
  val malloc_value_lazy : ?gc:bool -> 'a Memory_representation.t -> 'a -> 'a t
end = struct
  type 'a t =
    { address : Address.t lazy_t
    ; memory_representation : 'a Memory_representation.t
    }

  let address t = Lazy.force t.address
  let unsafe_cast t memory_representation = { t with memory_representation }

  let unsafe_of_raw address memory_representation =
    { address = lazy address; memory_representation }
  ;;

  let free t = Module.free (Lazy.force instance) (address t)
  let free_if_allocated t = if Lazy.is_val t.address then free t

  let repr_t memory_representation =
    Memory_representation.lift
      Memory_representation.address
      ~map:(fun t -> Lazy.force t.address)
      ~contramap:(fun address -> { memory_representation; address = lazy address })
  ;;

  let malloc_lazy ?(gc = true) (memory_representation : _ Memory_representation.t) n =
    let t =
      { address =
          lazy (Module.malloc (Lazy.force instance) (memory_representation.size * n))
      ; memory_representation
      }
    in
    if gc then Gc.finalise free_if_allocated t;
    t
  ;;

  let malloc ?gc (memory_representation : _ Memory_representation.t) n =
    let t = malloc_lazy ?gc memory_representation n in
    let (_ : Address.t) = address t in
    t
  ;;

  let get_indexed { address; memory_representation } n =
    (* TODO fix alignment *)
    memory_representation.setter_getter.get
      (Address.add (Lazy.force address) (memory_representation.size * n))
  ;;

  let get t = get_indexed t 0

  let set_indexed { address; memory_representation } v n =
    (* TODO fix alignment *)
    memory_representation.setter_getter.set
      (Address.add (Lazy.force address) (memory_representation.size * n))
      v
  ;;

  let set t v = set_indexed t v 0

  let malloc_value_lazy ?gc repr v =
    let t = malloc_lazy ?gc repr 1 in
    { t with
      address =
        Lazy.map
          (fun address ->
            set t v;
            address)
          t.address
    }
  ;;

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
  module Ptr_view = struct
    type ('repr, 'o) t =
      { to_js_arg : 'repr Memory_representation.t -> 'o -> Address.t
      ; finalize_arg : 'repr Pointer.t -> 'o -> unit
      ; of_js_return : 'repr Pointer.t -> 'o
      }

    let raw =
      { to_js_arg = (fun _repr pointer -> Pointer.address pointer)
      ; finalize_arg = (fun _ _ -> ())
      ; of_js_return = (fun ptr -> ptr)
      }
    ;;

    let sized =
      { to_js_arg =
          (fun repr v ->
            let ptr = Pointer.malloc ~gc:false repr (Array.length v) in
            Array.iteri v ~f:(fun i v -> Pointer.set_indexed ptr v i);
            Pointer.address ptr)
      ; finalize_arg =
          (fun ptr v ->
            Array.iteri v ~f:(fun i _ -> v.(i) <- Pointer.get_indexed ptr i);
            Pointer.free ptr)
      ; of_js_return =
          (fun _ptr ->
            failwith
              "Can't return a sized pointer; do not know the size. Return the raw \
               pointer and parse out the type yourself.")
      }
    ;;

    let const =
      { to_js_arg =
          (fun repr v -> Pointer.malloc_value ~gc:false repr v |> Pointer.address)
      ; finalize_arg = (fun ptr _v -> Pointer.free ptr)
      ; of_js_return =
          (fun ptr ->
            let result = Pointer.get ptr in
            Pointer.free ptr;
            result)
      }
    ;;

    let single =
      { to_js_arg =
          (fun repr v -> Pointer.malloc_value ~gc:false repr !v |> Pointer.address)
      ; finalize_arg =
          (fun ptr v ->
            v := Pointer.get ptr;
            Pointer.free ptr)
      ; of_js_return =
          (fun ptr ->
            let result = Pointer.get ptr in
            Pointer.free ptr;
            ref result)
      }
    ;;
  end

  type 'a t =
    | Primitive : 'a Primitive.t -> 'a t
    | Value : 'a Memory_representation.t -> 'a t
    | Pointer : 'a Memory_representation.t * ('a, 'o) Ptr_view.t -> 'o t
    | Void : unit t

  let to_emscripten_covariant (type a) (t : a t) =
    match t with
    | Primitive primitive -> Some (Primitive.to_ojs primitive)
    | Pointer (memory_representation, view) ->
      Some (fun v -> view.to_js_arg memory_representation v |> Address.t_to_js)
    | Value memory_representation ->
      Some
        (fun v ->
          Pointer.malloc_value ~gc:false memory_representation v
          |> Pointer.address
          |> Address.t_to_js)
    | Void -> None
  ;;

  let finally_of_emscripten_covariant (type a) (t : a t) =
    match t with
    | Primitive _ -> Some (fun _v _ojs -> ())
    | Void -> None
    | Pointer (memory_representation, view) ->
      Some
        (fun v ojs ->
          let ptr = Pointer.unsafe_of_raw (Address.t_of_js ojs) memory_representation in
          view.finalize_arg ptr v)
    | Value memory_representation ->
      Some
        (fun _v ojs ->
          let address = Address.t_of_js ojs in
          let pointer = Pointer.unsafe_of_raw address memory_representation in
          Pointer.free pointer)
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
      (a, ret) t
      -> arg_index:int
      -> name:string
      -> this:Ojs.t
      -> set:(Ojs.t array -> unit)
      -> finally:(Ojs.t array -> unit)
      -> a
    =
   fun t ~arg_index ~name ->
    match t with
    | Returning type_ ->
      let set_call_and_finalize ~this ~set ~finally =
        let args = Array.make arg_index Ojs.null in
        set args;
        let result = Ojs.call this name args in
        finally args;
        result, args
      in
      (match type_ with
       | Void ->
         fun ~this ~set ~finally -> set_call_and_finalize ~this ~set ~finally |> ignore
       | Primitive t ->
         let of_ojs = Primitive.of_ojs t in
         fun ~this ~set ~finally ->
           set_call_and_finalize ~this ~set ~finally |> fst |> of_ojs
       | Pointer (memory_representation, view) ->
         fun ~this ~set ~finally ->
           let address =
             set_call_and_finalize ~this ~set ~finally |> fst |> Address.t_of_js
           in
           Pointer.unsafe_of_raw address memory_representation |> view.of_js_return
       | Value memory_representation ->
         let of_pointer = Type.Ptr_view.const.of_js_return in
         fun ~this ~set ~finally ->
           let pointer = Pointer.malloc memory_representation 1 in
           let set args =
             args.(0) <- Pointer.address pointer |> Address.t_to_js;
             set args
           in
           set_call_and_finalize ~this ~set ~finally |> ignore;
           of_pointer pointer)
    | Abstract (arg, rest) ->
      (match
         Type.to_emscripten_covariant arg, Type.finally_of_emscripten_covariant arg
       with
       | Some set_v, Some finally_v ->
         let rest = build_arguments rest ~arg_index:(arg_index + 1) ~name in
         fun ~this ~set ~finally v ->
           let set args =
             args.(arg_index) <- set_v v;
             set args
           in
           let finally args =
             finally_v v args.(arg_index);
             finally args
           in
           rest ~this ~set ~finally
       | _ ->
         let rest = build_arguments rest ~arg_index ~name in
         fun ~this ~set ~finally _ -> rest ~this ~set ~finally)
 ;;

  (* Can't curry, maybe a list is needed? *)
  let extern (type a b ret) name (f : (a -> b, ret) t) =
    let return_type = return_type f in
    match return_type with
    | Value _ ->
      fun first_arg ->
        let this = Lazy.force instance |> Module.t_to_js in
        build_arguments f ~arg_index:1 ~name ~this ~set:ignore ~finally:ignore first_arg
    | _ ->
      fun first_arg ->
        let this = Lazy.force instance |> Module.t_to_js in
        build_arguments f ~arg_index:0 ~name ~this ~set:ignore ~finally:ignore first_arg
  ;;
end

module C_string = struct
  type t = string

  let strlen =
    Function.(
      extern
        "_strlen"
        (Pointer (Memory_representation.char, Type.Ptr_view.raw)
         @-> returning (Primitive UInt32)))
  ;;

  let ptr_of_string ?gc string =
    let pointer =
      Pointer.malloc ?gc Memory_representation.char (String.length string + 1)
    in
    String.iteri string ~f:(fun i char -> Pointer.set_indexed pointer char i);
    Pointer.set_indexed pointer '\000' (String.length string);
    pointer
  ;;

  let ptr_of_data_view ?gc (data_view : Data_view.t) =
    let t =
      Pointer.malloc ?gc Memory_representation.char (Data_view.byte_length data_view)
    in
    memcpy_dataview ~dst:(Pointer.address t) ~src:data_view;
    t
  ;;

  let ptr_to_string ptr =
    let length = strlen ptr in
    String.init length ~f:(fun i -> Pointer.get_indexed ptr i)
  ;;

  let view =
    { Type.Ptr_view.to_js_arg =
        (fun (_repr : char Memory_representation.t) string ->
          ptr_of_string ~gc:false string |> Pointer.address)
    ; finalize_arg = (fun ptr _ -> Pointer.free ptr)
    ; of_js_return =
        (fun ptr ->
          let string = ptr_to_string ptr in
          Pointer.free ptr;
          string)
    }
  ;;

  let t : t Type.t = Pointer (Memory_representation.char, view)
end

module Data_view_as_c_string = struct
  type t = Data_view.t

  let view_const =
    { Type.Ptr_view.to_js_arg =
        (fun repr data_view ->
          let ptr = Pointer.malloc ~gc:false repr (Data_view.byte_length data_view) in
          let address = Pointer.address ptr in
          memcpy_dataview ~dst:address ~src:data_view;
          address)
    ; finalize_arg = (fun ptr _ -> Pointer.free ptr)
    ; of_js_return =
        (fun _ -> failwith "Cannot deserialize data view from c string, unknown length")
    }
  ;;

  let t_const : t Type.t = Pointer (Memory_representation.char, view_const)
end
