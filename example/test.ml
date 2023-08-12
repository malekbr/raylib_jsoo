open! Base
open! Raylib_jsoo.Javascript_api
open! Raylib_jsoo.Emscripten
open! Raylib_jsoo.Raylib

module Promise = struct
  module T = struct
    type 'a t = 'a Promise.Any_error.t

    let return a = Promise.create (fun ~resolve ~reject:_ -> resolve a)
    let bind = Promise.bind
    let map = `Custom Promise.map
  end

  include T
  include Monad.Make (T)
end

open! Promise.Let_syntax

let () = Console.log [ Ojs.string_to_js "little_endian"; Ojs.bool_to_js little_endian ]

module Load_state = struct
  type t =
    { instance : Module.t
    ; watermelon : string
    }
end

let ready =
  let%map instance = ready
  and watermelon =
    let%bind response = fetch "watermelon.png" () in
    Response.array_buffer response
  in
  { Load_state.instance
  ; watermelon =
      (let view = Data_view.create watermelon in
       Console.log [ view ];
       String.init (Data_view.byte_length view) ~f:(fun byte_offset ->
         Data_view.get_uint8 view ~byte_offset |> Char.of_int_exn))
  }
;;

let (_ : unit Promise.t) =
  let%map { Load_state.instance; watermelon } = ready in
  let module _ = struct
    let () =
      Document.(query_selector t "canvas")
      |> Option.value_exn
      |> Module.set_canvas instance
    ;;

    let () = Console.log [ instance ]
    let vector = Vector2.add (Vector2.create 1. 2.) (Vector2.create 4. 5.)
    let () = Console.log [ vector.x; vector.y ]

    let () =
      Console.log
        [ C_string.(
            Pointer.unsafe_of_raw
              (view.to_js_arg Memory_representation.char "to c and back")
              Memory_representation.char
            |> view.of_js_return)
        ]
    ;;

    let () =
      set_config_flags (Config_flags.C_repr.Orable.of_ocaml_list [ MSAA_4X_HINT ]);
      init_window 800 600 "Test title";
      Console.log [ watermelon ];
      let watermelon_image =
        load_image_from_memory ".png" watermelon (String.length watermelon)
      in
      let watermelon_texture = load_texture_from_image watermelon_image in
      every_animation_frame 0. ~f:(fun rotation ->
        let delta_rotation = get_frame_time () *. 90. in
        begin_drawing ();
        clear_background Color.black;
        begin_mode_2d
          { target = { x = 400.; y = 300. }
          ; offset = { x = 400.; y = 300. }
          ; rotation
          ; zoom = 1.
          };
        draw_rectangle_rounded
          { x = 380.; y = 280.; width = 40.; height = 40. }
          0.5
          5
          Color.red;
        draw_texture watermelon_texture 460 (300 - 32) Color.white;
        end_mode_2d ();
        end_drawing ();
        `Continue (rotation +. delta_rotation))
    ;;
  end
  in
  ()
;;
