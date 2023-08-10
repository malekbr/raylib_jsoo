open! StdLabels
open! Javascript_api
open! Emscripten

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

module Config_flags = struct
  type t =
    | FULLSCREEN_MODE
    | MSAA_4X_HINT

  let all = [ FULLSCREEN_MODE; MSAA_4X_HINT ]

  let int_value = function
    | FULLSCREEN_MODE -> 0x2
    | MSAA_4X_HINT -> 0x20
  ;;

  module C_repr = (val Memory_representation.make_enum all ~int_value)
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

let set_config_flags =
  Function.(
    extern
      "_SetConfigFlags"
      (Primitive Config_flags.C_repr.Orable.primitive_t @-> returning Void))
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
