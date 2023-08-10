module Promise =
[%js:
type ('ok, 'error) t

val t_of_js : (Ojs.t -> 'a) -> (Ojs.t -> 'error) -> Ojs.t -> ('ok, 'error) t
val t_to_js : ('ok -> Ojs.t) -> ('error -> Ojs.t) -> ('ok, 'error) t -> Ojs.t

val create : (resolve:('a -> unit) -> reject:('error -> unit) -> unit) -> ('ok, 'error) t
  [@@js.new "Promise"]

val then_
  :  ('ok_in, 'err_in) t
  -> on_fulfilled:('ok_in -> ('ok_out, 'err_out) t)
  -> on_rejected:('err_in -> ('ok_out, 'err_out) t)
  -> ('ok_out, 'err_out) t
  [@@js.call "then"]

val map : ('a, 'error) t -> f:('a -> 'b) -> ('b, 'error) t [@@js.call "then"]
val bind : ('a, 'error) t -> f:('a -> ('b, 'error) t) -> ('b, 'error) t [@@js.call "then"]

module Any_error : sig
  type ('ok, 'error) outer = ('ok, 'error) t
  type 'ok t = ('ok, Ojs.t) outer

  val t_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'ok t
  val t_to_js : ('ok -> Ojs.t) -> 'ok t -> Ojs.t
end]

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

(*$ Cinaps_stubs.buffer_setters () *)
[@@@ocamlformat "disable"]
val get_int8 : t -> byte_offset:int -> int [@@js.call "getInt8"]
val set_int8 : t -> byte_offset:int -> value:int -> unit [@@js.call "setInt8"]
val get_uint8 : t -> byte_offset:int -> int [@@js.call "getUint8"]
val set_uint8 : t -> byte_offset:int -> value:int -> unit [@@js.call "setUint8"]
val get_int16 : t -> byte_offset:int -> little_endian:bool -> int [@@js.call "getInt16"]
val set_int16 : t -> byte_offset:int -> value:int -> little_endian:bool -> unit [@@js.call "setInt16"]
val get_uint16 : t -> byte_offset:int -> little_endian:bool -> int [@@js.call "getUint16"]
val set_uint16 : t -> byte_offset:int -> value:int -> little_endian:bool -> unit [@@js.call "setUint16"]
val get_int32 : t -> byte_offset:int -> little_endian:bool -> int [@@js.call "getInt32"]
val set_int32 : t -> byte_offset:int -> value:int -> little_endian:bool -> unit [@@js.call "setInt32"]
val get_uint32 : t -> byte_offset:int -> little_endian:bool -> int [@@js.call "getUint32"]
val set_uint32 : t -> byte_offset:int -> value:int -> little_endian:bool -> unit [@@js.call "setUint32"]
val get_int64 : t -> byte_offset:int -> little_endian:bool -> int [@@js.call "getInt64"]
val set_int64 : t -> byte_offset:int -> value:int -> little_endian:bool -> unit [@@js.call "setInt64"]
val get_uint64 : t -> byte_offset:int -> little_endian:bool -> int [@@js.call "getUint64"]
val set_uint64 : t -> byte_offset:int -> value:int -> little_endian:bool -> unit [@@js.call "setUint64"]
val get_float32 : t -> byte_offset:int -> little_endian:bool -> float [@@js.call "getFloat32"]
val set_float32 : t -> byte_offset:int -> value:float -> little_endian:bool -> unit [@@js.call "setFloat32"]
val get_float64 : t -> byte_offset:int -> little_endian:bool -> float [@@js.call "getFloat64"]
val set_float64 : t -> byte_offset:int -> value:float -> little_endian:bool -> unit [@@js.call "setFloat64"]
[@@@ocamlformat "enable"]
(*$*)

val byte_length : t -> int [@@js.get "byteLength"]
val byte_offset : t -> int [@@js.get "byteOffset"]]

module Response =
[%js:
type t

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t
val array_buffer : t -> Array_buffer.t Promise.Any_error.t [@@js.call "arrayBuffer"]]

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

include
  [%js:
  val fetch : string -> unit -> Response.t Promise.Any_error.t [@@js.global]

  val request_animation_frame : (unit -> unit) -> int
    [@@js.global "requestAnimationFrame"]]

module Typed_array =
[%js:
type 'a t

val t_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a t
val t_to_js : ('a -> Ojs.t) -> 'a t -> Ojs.t
val buffer : 'a t -> Array_buffer.t [@@js.get]

(*$ Cinaps_stubs.typed_array_creators () *)
[@@@ocamlformat "disable"]
val create_int8 : unit -> int t [@@js.new "Int8Array"]
val create_int8_length : int -> int t [@@js.new "Int8Array"]
val create_int8_from : ([`Typed_array of 'a t | `Array of int array | `List of int list][@js.union]) -> int t [@@js.new "Int8Array"]
val create_int8_buffer : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> int t [@@js.new "Int8Array"]
val create_uint8 : unit -> int t [@@js.new "Uint8Array"]
val create_uint8_length : int -> int t [@@js.new "Uint8Array"]
val create_uint8_from : ([`Typed_array of 'a t | `Array of int array | `List of int list][@js.union]) -> int t [@@js.new "Uint8Array"]
val create_uint8_buffer : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> int t [@@js.new "Uint8Array"]
val create_int16 : unit -> int t [@@js.new "Int16Array"]
val create_int16_length : int -> int t [@@js.new "Int16Array"]
val create_int16_from : ([`Typed_array of 'a t | `Array of int array | `List of int list][@js.union]) -> int t [@@js.new "Int16Array"]
val create_int16_buffer : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> int t [@@js.new "Int16Array"]
val create_uint16 : unit -> int t [@@js.new "Uint16Array"]
val create_uint16_length : int -> int t [@@js.new "Uint16Array"]
val create_uint16_from : ([`Typed_array of 'a t | `Array of int array | `List of int list][@js.union]) -> int t [@@js.new "Uint16Array"]
val create_uint16_buffer : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> int t [@@js.new "Uint16Array"]
val create_int32 : unit -> int t [@@js.new "Int32Array"]
val create_int32_length : int -> int t [@@js.new "Int32Array"]
val create_int32_from : ([`Typed_array of 'a t | `Array of int array | `List of int list][@js.union]) -> int t [@@js.new "Int32Array"]
val create_int32_buffer : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> int t [@@js.new "Int32Array"]
val create_uint32 : unit -> int t [@@js.new "Uint32Array"]
val create_uint32_length : int -> int t [@@js.new "Uint32Array"]
val create_uint32_from : ([`Typed_array of 'a t | `Array of int array | `List of int list][@js.union]) -> int t [@@js.new "Uint32Array"]
val create_uint32_buffer : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> int t [@@js.new "Uint32Array"]
val create_int64 : unit -> int t [@@js.new "Int64Array"]
val create_int64_length : int -> int t [@@js.new "Int64Array"]
val create_int64_from : ([`Typed_array of 'a t | `Array of int array | `List of int list][@js.union]) -> int t [@@js.new "Int64Array"]
val create_int64_buffer : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> int t [@@js.new "Int64Array"]
val create_uint64 : unit -> int t [@@js.new "Uint64Array"]
val create_uint64_length : int -> int t [@@js.new "Uint64Array"]
val create_uint64_from : ([`Typed_array of 'a t | `Array of int array | `List of int list][@js.union]) -> int t [@@js.new "Uint64Array"]
val create_uint64_buffer : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> int t [@@js.new "Uint64Array"]
val create_float32 : unit -> float t [@@js.new "Float32Array"]
val create_float32_length : int -> float t [@@js.new "Float32Array"]
val create_float32_from : ([`Typed_array of 'a t | `Array of float array | `List of float list][@js.union]) -> float t [@@js.new "Float32Array"]
val create_float32_buffer : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> float t [@@js.new "Float32Array"]
val create_float64 : unit -> float t [@@js.new "Float64Array"]
val create_float64_length : int -> float t [@@js.new "Float64Array"]
val create_float64_from : ([`Typed_array of 'a t | `Array of float array | `List of float list][@js.union]) -> float t [@@js.new "Float64Array"]
val create_float64_buffer : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> float t [@@js.new "Float64Array"]
[@@@ocamlformat "enable"]
(*$*)

val set
  :  'a t
  -> ([ `List of 'a list | `Array of 'a array | `Typed_array of 'b t ][@js.union])
  -> ?target_offset:int
  -> unit
  -> unit
  [@@js.call]]

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
