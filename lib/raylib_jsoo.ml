module Promise =
[%js:
type 'a t

val t_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a t
val t_to_js : ('a -> Ojs.t) -> 'a t -> Ojs.t

val create : (resolve:('a -> unit) -> reject:(Ojs.t -> unit) -> unit) -> 'a t
  [@@js.new "Promise"]

val then_ : 'a t -> f:('a -> 'b) -> 'b t [@@js.call "then"]
val map : 'a t -> f:('a -> 'b) -> 'b t [@@js.call "then"]
val bind : 'a t -> f:('a -> 'b t) -> 'b t [@@js.call "then"]]

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

module Array_buffer =
[%js:
type t

val t_of_js : Ojs.t -> t
val t_to_js : t -> Ojs.t]

module Data_view =
[%js:
type t

val t_of_js : Ojs.t -> t
val t_to_js : t -> Ojs.t
val create : Array_buffer.t -> t [@@js.new "DataView"]
val buffer : t -> Array_buffer.t [@@js.get]
val get_int8 : t -> byte_offset:Address.t -> int [@@js.call "getInt8"]
val set_int8 : t -> byte_offset:Address.t -> value:int -> unit [@@js.call "setInt8"]
val get_uint8 : t -> byte_offset:Address.t -> int [@@js.call "getUint8"]
val set_uint8 : t -> byte_offset:Address.t -> value:int -> unit [@@js.call "setUint8"]

val get_int16 : t -> byte_offset:Address.t -> little_endian:bool -> int
  [@@js.call "getInt16"]

val set_int16 : t -> byte_offset:Address.t -> value:int -> little_endian:bool -> unit
  [@@js.call "setInt16"]

val get_uint16 : t -> byte_offset:Address.t -> little_endian:bool -> int
  [@@js.call "getUint16"]

val set_uint16 : t -> byte_offset:Address.t -> value:int -> little_endian:bool -> unit
  [@@js.call "setUint16"]

val get_int32 : t -> byte_offset:Address.t -> little_endian:bool -> int
  [@@js.call "getInt32"]

val set_int32 : t -> byte_offset:Address.t -> value:int -> little_endian:bool -> unit
  [@@js.call "setInt32"]

val get_uint32 : t -> byte_offset:Address.t -> little_endian:bool -> int
  [@@js.call "getUint32"]

val set_uint32 : t -> byte_offset:Address.t -> value:int -> little_endian:bool -> unit
  [@@js.call "setUint32"]

val get_float32 : t -> byte_offset:Address.t -> little_endian:bool -> float
  [@@js.call "getFloat32"]

val set_float32 : t -> byte_offset:Address.t -> value:float -> little_endian:bool -> unit
  [@@js.call "setFloat32"]

val get_float64 : t -> byte_offset:Address.t -> little_endian:bool -> float
  [@@js.call "getFloat64"]

val set_float64 : t -> byte_offset:Address.t -> value:float -> little_endian:bool -> unit
  [@@js.call "setFloat64"]

val byte_length : t -> int [@@js.get "byteLength"]
val byte_offset : t -> int [@@js.get "byteOffset"]
(**)]

module Response =
[%js:
type t

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t
val array_buffer : t -> Array_buffer.t Promise.t [@@js.call "arrayBuffer"]]

include
  [%js:
  val fetch : string -> unit -> Response.t Promise.t [@@js.global]

  val request_animation_frame : (unit -> unit) -> int
    [@@js.global "requestAnimationFrame"]]

let every_animation_frame init ~f =
  let acc = ref init in
  let rec loop () =
    match f !acc with
    | `Stop -> ()
    | `Continue v ->
      acc := v;
      request_animation_frame loop |> ignore
  in
  request_animation_frame loop |> ignore
;;

module Element =
[%js:
type t

val t_of_js : Ojs.t -> t
val t_to_js : t -> Ojs.t]

module Document =
[%js:
type t

val t_of_js : Ojs.t -> t
val t_to_js : t -> Ojs.t
val t : t [@@js.global "document"]
val query_selector : t -> string -> Element.t option [@@js.call "querySelector"]]

module Sized_int_array =
[%js:
type t

val create_uint16 : int -> t [@@js.new "Uint16Array"]

val create_uint8 : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> t
  [@@js.new "Uint8Array"]

val buffer : t -> Array_buffer.t [@@js.get]
val t_of_js : Ojs.t -> t
val set_buffer : t -> src:t -> ?byte_offset:int -> unit -> unit [@@js.call "set"]
val set : t -> int list -> unit [@@js.call]]

let is_small_endian =
  let sized_array = Sized_int_array.create_uint16 1 in
  Sized_int_array.set sized_array [ 0x1122 ];
  let bytes = Data_view.create (Sized_int_array.buffer sized_array) in
  match Data_view.get_uint8 bytes ~byte_offset:Address.zero with
  | 0x11 -> false
  | 0x22 -> true
  | _ -> assert false
;;

module Sized_array =
[%js:
type 'a t = private Ojs.t

val t_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a t
val get : 'a t -> Address.t -> 'a [@@js.call]
val set : 'a t -> Address.t -> 'a -> unit [@@js.call]]

module Nothing = struct
  type t = |

  let unreachable : t -> 'a = function
    | _ -> .
  ;;

  let t_to_js = unreachable
end

module FS =
[%js:
type t

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

module Dir : sig
  type t
end

module File : sig
  type t
end

val mkdir : t -> path:string -> ?mode:int -> unit -> Dir.t [@@js.call]

val write_file
  :  t
  -> path:string
  -> data:([ `String of string | `Data_view of Data_view.t ][@js.union])
  -> ?opts:Nothing.t
  -> unit
  -> unit
  [@@js.call "writeFile"]]

module Emcc =
[%js:
type t

val t_to_js : t -> Ojs.t
val create_module : unit -> t Promise.t [@@js.global "createModule"]
val heap : t -> Sized_int_array.t [@@js.get "HEAP8"]
val malloc : t -> int -> Address.t [@@js.call "_malloc"]
val string_to_new_utf8 : t -> string -> Address.t [@@js.call "stringToNewUTF8"]
val utf8_to_string : t -> Address.t -> string [@@js.call "UTF8ToString"]
val free : t -> Address.t -> unit [@@js.call "_free"]
val fs : t -> FS.t [@@js.get "FS"]
val set_canvas : t -> Element.t -> unit [@@js.set]]

let heap = ref None
let emcc_instance = ref None
let emcc = Emcc.create_module ()

module Console = struct
  module Args = struct
    type t =
      | [] : t
      | ( :: ) : 'a * t -> t

    let rec to_list : t -> Js_of_ocaml.Js.Unsafe.any list = function
      | [] -> []
      | a :: rest -> Js_of_ocaml.Js.Unsafe.inject a :: to_list rest
    ;;
  end

  let console = Js_of_ocaml.Js.Unsafe.eval_string {|console|}

  let log args : unit =
    Args.to_list args |> Array.of_list |> Js_of_ocaml.Js.Unsafe.meth_call console "log"
  ;;
end

let (ready : Emcc.t Promise.t) =
  Promise.then_ emcc ~f:(fun emcc ->
    heap := Some (Emcc.heap emcc);
    emcc_instance := Some emcc;
    emcc)
;;

let instance = lazy (Option.get !emcc_instance)
let heap = lazy (Option.get !heap |> Sized_int_array.buffer |> Data_view.create)

let memcpy_dataview ~(dst : Address.t) ~src =
  let heap = Lazy.force instance |> Emcc.heap in
  let src =
    Sized_int_array.create_uint8
      (Data_view.buffer src)
      ~length:(Data_view.byte_length src)
      ~byte_offset:(Data_view.byte_offset src)
      ()
  in
  Sized_int_array.set_buffer heap ~src ~byte_offset:(dst :> int) ()
;;

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
          ~set:(fun byte_offset value ->
            Data_view.set_int8 (Lazy.force heap) ~byte_offset ~value)
          ~get:(fun byte_offset -> Data_view.get_int8 (Lazy.force heap) ~byte_offset)
    ; size = 1
    ; alignment = 1
    }
  ;;

  let uint8_t =
    { setter_getter =
        Setter_getter.base
          ~set:(fun byte_offset value ->
            Data_view.set_uint8 (Lazy.force heap) ~byte_offset ~value)
          ~get:(fun byte_offset -> Data_view.get_uint8 (Lazy.force heap) ~byte_offset)
    ; size = 1
    ; alignment = 1
    }
  ;;

  let[@inline always] setter f byte_offset value =
    f (Lazy.force heap) ~byte_offset ~value ~little_endian:is_small_endian
  ;;

  let[@inline always] getter f byte_offset =
    f (Lazy.force heap) ~byte_offset ~little_endian:is_small_endian
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
  let free t = Emcc.free (Lazy.force instance) t.address

  let repr_t memory_representation =
    Memory_representation.lift
      Memory_representation.address
      ~map:(fun t -> t.address)
      ~contramap:(fun address -> { memory_representation; address })
  ;;

  let malloc ?(gc = true) (memory_representation : _ Memory_representation.t) n =
    let t =
      { address = Emcc.malloc (Lazy.force instance) (memory_representation.size * n)
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
        let this = Lazy.force instance |> Emcc.t_to_js in
        let args = Array.make (arg_count + 1) Ojs.null in
        build_arguments f ~arg_index:1 ~name ~args ~this first_arg
    | _ ->
      fun first_arg ->
        let this = Lazy.force instance |> Emcc.t_to_js in
        let args = Array.make arg_count Ojs.null in
        build_arguments f ~arg_index:0 ~name first_arg ~this ~args
  ;;
end

module C_string = struct
  type t = char Pointer.t

  let t : t Type.t = Pointer Memory_representation.char

  let of_string ?(gc = true) string =
    let address = Emcc.string_to_new_utf8 (Lazy.force instance) string in
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

  let to_string (t : t) = Emcc.utf8_to_string (Lazy.force instance) (Pointer.address t)
end

module Color = struct
  open! Memory_representation
  open! Structure

  type t =
    { r : int
    ; g : int
    ; b : int
    ; a : int
    }

  let r, repr_t = field empty_struct uint8_t
  let g, repr_t = field repr_t uint8_t
  let b, repr_t = field repr_t uint8_t
  let a, repr_t = field repr_t uint8_t

  let repr_t =
    lift
      repr_t
      ~map:(fun { r; g; b; a } -> ((((), r), g), b), a)
      ~contramap:(fun (((((), r), g), b), a) -> { r; g; b; a })
  ;;

  let red = { r = 255; g = 0; b = 0; a = 255 }
  let black = { r = 0; g = 0; b = 0; a = 255 }
  let white = { r = 255; g = 255; b = 255; a = 255 }
end

module Vector2 = struct
  open! Memory_representation
  open! Structure

  type t =
    { x : float
    ; y : float
    }

  let zero = { x = 0.; y = 0. }
  let x, repr_t = field empty_struct float32_t
  let y, repr_t = field repr_t float32_t

  let repr_t =
    lift
      repr_t
      ~map:(fun { x; y } -> ((), x), y)
      ~contramap:(fun (((), x), y) -> { x; y })
  ;;
end

module Camera2D = struct
  open! Memory_representation
  open! Structure

  type t =
    { offset : Vector2.t
    ; target : Vector2.t
    ; rotation : float (* Degrees *)
    ; zoom : float (* Scaling *)
    }

  let offset, repr_t = field empty_struct Vector2.repr_t
  let target, repr_t = field repr_t Vector2.repr_t
  let rotation, repr_t = field repr_t float32_t
  let zoom, repr_t = field repr_t float32_t

  let repr_t =
    lift
      repr_t
      ~map:(fun { offset; target; rotation; zoom } ->
        ((((), offset), target), rotation), zoom)
      ~contramap:(fun (((((), offset), target), rotation), zoom) ->
        { offset; target; rotation; zoom })
  ;;
end

module Rectangle = struct
  open! Memory_representation
  open! Structure

  type t =
    { x : float
    ; y : float
    ; width : float
    ; height : float
    }

  let x, repr_t = field empty_struct float32_t
  let y, repr_t = field repr_t float32_t
  let width, repr_t = field repr_t float32_t
  let height, repr_t = field repr_t float32_t

  let repr_t =
    lift
      repr_t
      ~map:(fun { x; y; width; height } -> ((((), x), y), width), height)
      ~contramap:(fun (((((), x), y), width), height) -> { x; y; width; height })
  ;;
end

module Image = struct
  open! Memory_representation
  open! Structure

  type t =
    { data : C_string.t
    ; width : int
    ; height : int
    ; mipmaps : int
    ; format : int
    }

  let data, repr_t = field empty_struct (Pointer.repr_t Memory_representation.char)
  let width, repr_t = field repr_t int32_t
  let height, repr_t = field repr_t int32_t
  let mipmaps, repr_t = field repr_t int32_t
  let format, repr_t = field repr_t int32_t

  let repr_t =
    lift
      repr_t
      ~map:(fun { data; width; height; mipmaps; format } ->
        (((((), data), width), height), mipmaps), format)
      ~contramap:(fun ((((((), data), width), height), mipmaps), format) ->
        { data; width; height; mipmaps; format })
  ;;
end

module Texture2D = struct
  open! Memory_representation
  open! Structure

  type t =
    { id : int
    ; width : int
    ; height : int
    ; mipmaps : int
    ; format : int
    }

  let id, repr_t = field empty_struct uint32_t
  let width, repr_t = field repr_t int32_t
  let height, repr_t = field repr_t int32_t
  let mipmaps, repr_t = field repr_t int32_t
  let format, repr_t = field repr_t int32_t

  let repr_t =
    lift
      repr_t
      ~map:(fun { id; width; height; mipmaps; format } ->
        (((((), id), width), height), mipmaps), format)
      ~contramap:(fun ((((((), id), width), height), mipmaps), format) ->
        { id; width; height; mipmaps; format })
  ;;
end

let vector2_add =
  Function.(
    extern
      "_Vector2Add"
      (Value Vector2.repr_t @-> Value Vector2.repr_t @-> returning (Value Vector2.repr_t)))
;;

let init_window =
  Function.(
    extern
      "_InitWindow"
      (Primitive Int32 @-> Primitive Int32 @-> C_string.t @-> returning Void))
;;

let begin_drawing = Function.(extern "_BeginDrawing" (Void @-> returning Void))
let end_drawing = Function.(extern "_EndDrawing" (Void @-> returning Void))

let draw_rectangle =
  Function.(
    extern
      "_DrawRectangle"
      (Primitive Int32
       @-> Primitive Int32
       @-> Primitive Int32
       @-> Primitive Int32
       @-> Value Color.repr_t
       @-> returning Void))
;;

let begin_mode_2d =
  Function.(extern "_BeginMode2D" (Value Camera2D.repr_t @-> returning Void))
;;

let end_mode_2d = Function.(extern "_EndMode2D" (Void @-> returning Void))

let get_frame_time =
  Function.(extern "_GetFrameTime" (Void @-> returning (Primitive F32)))
;;

let draw_rectangle_rounded =
  Function.(
    extern
      "_DrawRectangleRounded"
      (Value Rectangle.repr_t
       @-> Primitive F32
       @-> Primitive Int32
       @-> Value Color.repr_t
       @-> returning Void))
;;

let clear_background =
  Function.(extern "_ClearBackground" (Value Color.repr_t @-> returning Void))
;;

let load_texture =
  Function.(extern "_LoadTexture" (C_string.t @-> returning (Value Texture2D.repr_t)))
;;

let load_texture_from_image =
  Function.(
    extern
      "_LoadTextureFromImage"
      (Value Image.repr_t @-> returning (Value Texture2D.repr_t)))
;;

let load_image =
  Function.(extern "_LoadImage" (C_string.t @-> returning (Value Image.repr_t)))
;;

let load_image_from_memory =
  Function.(
    extern
      "_LoadImageFromMemory"
      (C_string.t @-> C_string.t @-> Primitive UInt32 @-> returning (Value Image.repr_t)))
;;

let draw_texture =
  Function.(
    extern
      "_DrawTexture"
      (Value Texture2D.repr_t
       @-> Primitive Int32
       @-> Primitive Int32
       @-> Value Color.repr_t
       @-> returning Void))
;;
