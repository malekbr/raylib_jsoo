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
    ; watermelon : Data_view.t
    }
end

let ready =
  let%map instance = ready
  and watermelon =
    let%bind response = fetch "watermelon.png" () in
    Response.array_buffer response
  in
  { Load_state.instance; watermelon = Data_view.create watermelon }
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
    let vector = Vector2.add (Vector2.create 1. 2.) (Vector2.create 4. 5.) |> Pointer.get
    let () = Console.log [ vector.x; vector.y ]
    let () = Console.log [ C_string.(of_string "to c and back" |> to_string) ]

    let () =
      set_config_flags (Config_flags.C_repr.Orable.of_ocaml_list [ MSAA_4X_HINT ]);
      init_window 800 600 (C_string.of_string "Test title");
      let watermelon_image =
        load_image_from_memory
          (C_string.of_string ".png")
          (C_string.of_data_view watermelon)
          (Data_view.byte_length watermelon)
      in
      let watermelon_texture = load_texture_from_image watermelon_image in
      every_animation_frame 0. ~f:(fun rotation ->
        let delta_rotation = get_frame_time () *. 90. in
        begin_drawing ();
        clear_background (Pointer.malloc_value Color.repr_t Color.black);
        begin_mode_2d
          (Pointer.malloc_value
             Camera2D.repr_t
             { target = { x = 400.; y = 300. }
             ; offset = { x = 400.; y = 300. }
             ; rotation
             ; zoom = 1.
             });
        draw_rectangle_rounded
          (Pointer.malloc_value
             Rectangle.repr_t
             { x = 380.; y = 280.; width = 40.; height = 40. })
          0.5
          5
          (Pointer.malloc_value Color.repr_t Color.red);
        draw_texture
          watermelon_texture
          460
          (300 - 32)
          (Pointer.malloc_value Color.repr_t Color.white);
        end_mode_2d ();
        end_drawing ();
        `Continue (rotation +. delta_rotation))
    ;;
  end
  in
  ()
;;
