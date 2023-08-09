open! Base
open! Raylib_jsoo

let on_runtime_initialized f = ignore (Promise.then_ ready f : unit Promise.t)

let () =
  Console.log [ Ojs.string_to_js "is_small_endian"; Ojs.bool_to_js is_small_endian ]
;;

let () =
  on_runtime_initialized (fun emcc ->
    let module _ = struct
      let () =
        Document.(query_selector t "canvas") |> Option.value_exn |> Emcc.set_canvas emcc
      ;;

      let () = Console.log [ emcc ]

      let vector =
        vector2_add
          (Pointer.malloc_value Vector2.repr_t { x = 1.; y = 2. })
          (Pointer.malloc_value Vector2.repr_t { x = 4.; y = 5. })
        |> Pointer.get
      ;;

      let () = Console.log [ vector.x; vector.y ]
      let () = Console.log [ C_string.(of_string "to c and back" |> to_string) ]

      let () =
        init_window 800 600 (C_string.of_string "Test title");
        every_animation_frame 0. ~f:(fun rotation ->
          begin_drawing ();
          begin_mode_2d
            (Pointer.malloc_value
               Camera2D.repr_t
               { target = { x = 400.; y = 300. }
               ; offset = { x = 400.; y = 300. }
               ; rotation
               ; zoom = 1.
               });
          draw_rectangle 380 280 40 40 (Pointer.malloc_value Color.repr_t Color.red);
          end_mode_2d ();
          end_drawing ();
          `Continue (rotation +. 1.))
      ;;
    end
    in
    ())
;;
