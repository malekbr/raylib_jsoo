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

  let _r, repr_t = field empty_struct uint8_t
  let _g, repr_t = field repr_t uint8_t
  let _b, repr_t = field repr_t uint8_t
  let _a, repr_t = field repr_t uint8_t

  let repr_t =
    lift
      repr_t
      ~map:(fun { r; g; b; a } -> ((((), r), g), b), a)
      ~contramap:(fun (((((), r), g), b), a) -> { r; g; b; a })
  ;;

  let create r g b a = { r; g; b; a }
  let lightgray = { r = 200; g = 200; b = 200; a = 255 }
  let gray = { r = 130; g = 130; b = 130; a = 255 }
  let darkgray = { r = 80; g = 80; b = 80; a = 255 }
  let yellow = { r = 253; g = 249; b = 0; a = 255 }
  let gold = { r = 255; g = 203; b = 0; a = 255 }
  let orange = { r = 255; g = 161; b = 0; a = 255 }
  let pink = { r = 255; g = 109; b = 194; a = 255 }
  let red = { r = 230; g = 41; b = 55; a = 255 }
  let maroon = { r = 190; g = 33; b = 55; a = 255 }
  let green = { r = 0; g = 228; b = 48; a = 255 }
  let lime = { r = 0; g = 158; b = 47; a = 255 }
  let darkgreen = { r = 0; g = 117; b = 44; a = 255 }
  let skyblue = { r = 102; g = 191; b = 255; a = 255 }
  let blue = { r = 0; g = 121; b = 241; a = 255 }
  let darkblue = { r = 0; g = 82; b = 172; a = 255 }
  let purple = { r = 200; g = 122; b = 255; a = 255 }
  let violet = { r = 135; g = 60; b = 190; a = 255 }
  let darkpurple = { r = 112; g = 31; b = 126; a = 255 }
  let beige = { r = 211; g = 176; b = 131; a = 255 }
  let brown = { r = 127; g = 106; b = 79; a = 255 }
  let darkbrown = { r = 76; g = 63; b = 47; a = 255 }
  let white = { r = 255; g = 255; b = 255; a = 255 }
  let black = { r = 0; g = 0; b = 0; a = 255 }
  let blank = { r = 0; g = 0; b = 0; a = 0 }
  let magenta = { r = 255; g = 0; b = 255; a = 255 }
  let raywhite = { r = 245; g = 245; b = 245; a = 255 }
end

module Vector2 = struct
  open! Memory_representation
  open! Structure

  type t =
    { x : float
    ; y : float
    }

  let _x, repr_t = field empty_struct float32_t
  let _y, repr_t = field repr_t float32_t

  let repr_t =
    lift
      repr_t
      ~map:(fun { x; y } -> ((), x), y)
      ~contramap:(fun (((), x), y) -> { x; y })
  ;;

  let create x y = { x; y }
  let x (t : t) = t.x
  let y (t : t) = t.y

  let add =
    Function.(
      extern "_Vector2Add" (Value repr_t @-> Value repr_t @-> returning (Value repr_t)))
  ;;

  let subtract =
    Function.(
      extern
        "_Vector2Subtract"
        (Value repr_t @-> Value repr_t @-> returning (Value repr_t)))
  ;;

  let add_value : t -> float -> t =
    Function.(
      extern
        "_Vector2AddValue"
        (Value repr_t @-> Primitive F32 @-> returning (Value repr_t)))
  ;;

  let scale : t -> float -> t =
    Function.(
      extern "_Vector2Scale" (Value repr_t @-> Primitive F32 @-> returning (Value repr_t)))
  ;;

  let normalize : t -> t =
    Function.(extern "_Vector2Normalize" (Value repr_t @-> returning (Value repr_t)))
  ;;

  let zero = Function.(extern "_Vector2Zero" (Void @-> returning (Value repr_t)))
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

  let _offset, repr_t = field empty_struct Vector2.repr_t
  let _target, repr_t = field repr_t Vector2.repr_t
  let _rotation, repr_t = field repr_t float32_t
  let _zoom, repr_t = field repr_t float32_t

  let repr_t =
    lift
      repr_t
      ~map:(fun { offset; target; rotation; zoom } ->
        ((((), offset), target), rotation), zoom)
      ~contramap:(fun (((((), offset), target), rotation), zoom) ->
        { offset; target; rotation; zoom })
  ;;

  let create offset target rotation zoom = { offset; target; rotation; zoom }
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

  let _x, repr_t = field empty_struct float32_t
  let _y, repr_t = field repr_t float32_t
  let _width, repr_t = field repr_t float32_t
  let _height, repr_t = field repr_t float32_t

  let repr_t =
    lift
      repr_t
      ~map:(fun { x; y; width; height } -> ((((), x), y), width), height)
      ~contramap:(fun (((((), x), y), width), height) -> { x; y; width; height })
  ;;

  let create x y width height = { x; y; width; height }
  let x (t : t) = t.x
  let y (t : t) = t.y
  let width (t : t) = t.width
  let height (t : t) = t.height
end

module Image = struct
  open! Memory_representation
  open! Structure

  type t =
    { data : char Pointer.t
    ; width : int
    ; height : int
    ; mipmaps : int
    ; format : int
    }

  let _data, repr_t = field empty_struct (Pointer.repr_t Memory_representation.char)
  let _width, repr_t = field repr_t int32_t
  let _height, repr_t = field repr_t int32_t
  let _mipmaps, repr_t = field repr_t int32_t
  let _format, repr_t = field repr_t int32_t

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

  let _id, repr_t = field empty_struct uint32_t
  let _width, repr_t = field repr_t int32_t
  let _height, repr_t = field repr_t int32_t
  let _mipmaps, repr_t = field repr_t int32_t
  let _format, repr_t = field repr_t int32_t

  let repr_t =
    lift
      repr_t
      ~map:(fun { id; width; height; mipmaps; format } ->
        (((((), id), width), height), mipmaps), format)
      ~contramap:(fun ((((((), id), width), height), mipmaps), format) ->
        { id; width; height; mipmaps; format })
  ;;

  let width (t : t) = t.width
  let height (t : t) = t.height
end

module Key = struct
  type t =
    | Null
    | Apostrophe
    | Comma
    | Minus
    | Period
    | Slash
    | Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Semicolon
    | Equal
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z
    | Left_bracket
    | Backslash
    | Right_bracket
    | Grave
    | Space
    | Escape
    | Enter
    | Tab
    | Backspace
    | Insert
    | Delete
    | Right
    | Left
    | Down
    | Up
    | Page_up
    | Page_down
    | Home
    | End
    | Caps_lock
    | Scroll_lock
    | Num_lock
    | Print_screen
    | Pause
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | Left_shift
    | Left_control
    | Left_alt
    | Left_super
    | Right_shift
    | Right_control
    | Right_alt
    | Right_super
    | Kb_menu
    | Kp_0
    | Kp_1
    | Kp_2
    | Kp_3
    | Kp_4
    | Kp_5
    | Kp_6
    | Kp_7
    | Kp_8
    | Kp_9
    | Kp_decimal
    | Kp_divide
    | Kp_multiply
    | Kp_subtract
    | Kp_add
    | Kp_enter
    | Kp_equal
    | Back
    | Menu
    | Volume_up
    | Volume_down

  let all =
    [ Null
    ; Apostrophe
    ; Comma
    ; Minus
    ; Period
    ; Slash
    ; Zero
    ; One
    ; Two
    ; Three
    ; Four
    ; Five
    ; Six
    ; Seven
    ; Eight
    ; Nine
    ; Semicolon
    ; Equal
    ; A
    ; B
    ; C
    ; D
    ; E
    ; F
    ; G
    ; H
    ; I
    ; J
    ; K
    ; L
    ; M
    ; N
    ; O
    ; P
    ; Q
    ; R
    ; S
    ; T
    ; U
    ; V
    ; W
    ; X
    ; Y
    ; Z
    ; Left_bracket
    ; Backslash
    ; Right_bracket
    ; Grave
    ; Space
    ; Escape
    ; Enter
    ; Tab
    ; Backspace
    ; Insert
    ; Delete
    ; Right
    ; Left
    ; Down
    ; Up
    ; Page_up
    ; Page_down
    ; Home
    ; End
    ; Caps_lock
    ; Scroll_lock
    ; Num_lock
    ; Print_screen
    ; Pause
    ; F1
    ; F2
    ; F3
    ; F4
    ; F5
    ; F6
    ; F7
    ; F8
    ; F9
    ; F10
    ; F11
    ; F12
    ; Left_shift
    ; Left_control
    ; Left_alt
    ; Left_super
    ; Right_shift
    ; Right_control
    ; Right_alt
    ; Right_super
    ; Kb_menu
    ; Kp_0
    ; Kp_1
    ; Kp_2
    ; Kp_3
    ; Kp_4
    ; Kp_5
    ; Kp_6
    ; Kp_7
    ; Kp_8
    ; Kp_9
    ; Kp_decimal
    ; Kp_divide
    ; Kp_multiply
    ; Kp_subtract
    ; Kp_add
    ; Kp_enter
    ; Kp_equal
    ; Back
    ; Menu
    ; Volume_up
    ; Volume_down
    ]
  ;;

  let int_value = function
    | Null -> 0
    | Apostrophe -> 39
    | Comma -> 44
    | Minus -> 45
    | Period -> 46
    | Slash -> 47
    | Zero -> 48
    | One -> 49
    | Two -> 50
    | Three -> 51
    | Four -> 52
    | Five -> 53
    | Six -> 54
    | Seven -> 55
    | Eight -> 56
    | Nine -> 57
    | Semicolon -> 59
    | Equal -> 61
    | A -> 65
    | B -> 66
    | C -> 67
    | D -> 68
    | E -> 69
    | F -> 70
    | G -> 71
    | H -> 72
    | I -> 73
    | J -> 74
    | K -> 75
    | L -> 76
    | M -> 77
    | N -> 78
    | O -> 79
    | P -> 80
    | Q -> 81
    | R -> 82
    | S -> 83
    | T -> 84
    | U -> 85
    | V -> 86
    | W -> 87
    | X -> 88
    | Y -> 89
    | Z -> 90
    | Left_bracket -> 91
    | Backslash -> 92
    | Right_bracket -> 93
    | Grave -> 96
    | Space -> 32
    | Escape -> 256
    | Enter -> 257
    | Tab -> 258
    | Backspace -> 259
    | Insert -> 260
    | Delete -> 261
    | Right -> 262
    | Left -> 263
    | Down -> 264
    | Up -> 265
    | Page_up -> 266
    | Page_down -> 267
    | Home -> 268
    | End -> 269
    | Caps_lock -> 280
    | Scroll_lock -> 281
    | Num_lock -> 282
    | Print_screen -> 283
    | Pause -> 284
    | F1 -> 290
    | F2 -> 291
    | F3 -> 292
    | F4 -> 293
    | F5 -> 294
    | F6 -> 295
    | F7 -> 296
    | F8 -> 297
    | F9 -> 298
    | F10 -> 299
    | F11 -> 300
    | F12 -> 301
    | Left_shift -> 340
    | Left_control -> 341
    | Left_alt -> 342
    | Left_super -> 343
    | Right_shift -> 344
    | Right_control -> 345
    | Right_alt -> 346
    | Right_super -> 347
    | Kb_menu -> 348
    | Kp_0 -> 320
    | Kp_1 -> 321
    | Kp_2 -> 322
    | Kp_3 -> 323
    | Kp_4 -> 324
    | Kp_5 -> 325
    | Kp_6 -> 326
    | Kp_7 -> 327
    | Kp_8 -> 328
    | Kp_9 -> 329
    | Kp_decimal -> 330
    | Kp_divide -> 331
    | Kp_multiply -> 332
    | Kp_subtract -> 333
    | Kp_add -> 334
    | Kp_enter -> 335
    | Kp_equal -> 336
    | Back -> 4
    | Menu -> 82
    | Volume_up -> 24
    | Volume_down -> 25
  ;;

  module C_repr = (val Memory_representation.make_enum ~int_value all)
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

let draw_texture_pro =
  Function.(
    extern
      "_DrawTexturePro"
      (Value Texture2D.repr_t
       @-> Value Rectangle.repr_t
       @-> Value Rectangle.repr_t
       @-> Value Vector2.repr_t
       @-> Primitive F32
       @-> Value Color.repr_t
       @-> returning Void))
;;

let draw_rectangle_rec =
  Function.(
    extern
      "_DrawRectangleRec"
      (Value Rectangle.repr_t @-> Value Color.repr_t @-> returning Void))
;;

let draw_circle_v =
  Function.(
    extern
      "_DrawCircleV"
      (Value Vector2.repr_t @-> Primitive F32 @-> Value Color.repr_t @-> returning Void))
;;

let draw_triangle =
  Function.(
    extern
      "_DrawTriangle"
      (Value Vector2.repr_t
       @-> Value Vector2.repr_t
       @-> Value Vector2.repr_t
       @-> Value Color.repr_t
       @-> returning Void))
;;

let draw_line_bezier_quad =
  Function.(
    extern
      "_DrawLineBezierQuad"
      (Value Vector2.repr_t
       @-> Value Vector2.repr_t
       @-> Value Vector2.repr_t
       @-> Primitive F32
       @-> Value Color.repr_t
       @-> returning Void))
;;

let is_key_pressed =
  Function.(
    extern
      "_IsKeyPressed"
      (Primitive Key.C_repr.ocaml_primitive_t @-> returning (Primitive Primitive.bool)))
;;

let get_key_pressed =
  Function.(
    extern "_GetKeyPressed" (Void @-> returning (Primitive Key.C_repr.ocaml_primitive_t)))
;;
