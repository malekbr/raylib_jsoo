open! StdLabels

module Type = struct
  type t =
    { js_name : string
    ; ocaml_name : string
    ; ocaml_type : string
    }

  let multi_byte =
    List.map
      [ "Int16", "int16", "int"
      ; "Uint16", "uint16", "int"
      ; "Int32", "int32", "int"
      ; "Uint32", "uint32", "int"
      ; "Int64", "int64", "int"
      ; "Uint64", "uint64", "int"
      ; "Float32", "float32", "float"
      ; "Float64", "float64", "float"
      ]
      ~f:(fun (js_name, ocaml_name, ocaml_type) -> { js_name; ocaml_name; ocaml_type })
  ;;

  let single_byte =
    List.map
      [ "Int8", "int8", "int"; "Uint8", "uint8", "int" ]
      ~f:(fun (js_name, ocaml_name, ocaml_type) -> { js_name; ocaml_name; ocaml_type })
  ;;

  let all = single_byte @ multi_byte
end

let typed_array_creator { Type.js_name; ocaml_type; ocaml_name } =
  Printf.printf
    {|val create_%s : unit -> %s t [@@js.new "%sArray"]|}
    ocaml_name
    ocaml_type
    js_name;
  print_newline ();
  Printf.printf
    {|val create_%s_length : int -> %s t [@@js.new "%sArray"]|}
    ocaml_name
    ocaml_type
    js_name;
  print_newline ();
  Printf.printf
    {|val create_%s_from : ([`Typed_array of 'a t | `Array of %s array | `List of %s list][@js.union]) -> %s t [@@js.new "%sArray"]|}
    ocaml_name
    ocaml_type
    ocaml_type
    ocaml_type
    js_name;
  print_newline ();
  Printf.printf
    {|val create_%s_buffer : Array_buffer.t -> ?byte_offset:int -> ?length:int -> unit -> %s t [@@js.new "%sArray"]|}
    ocaml_name
    ocaml_type
    js_name;
  print_newline ()
;;

let with_formatting ~f =
  print_newline ();
  print_endline {|[@@@ocamlformat "disable"]|};
  f ();
  print_endline {|[@@@ocamlformat "enable"]|}
;;

let typed_array_creators () =
  with_formatting ~f:(fun () -> List.iter Type.all ~f:typed_array_creator)
;;

let buffer_setter ~multi_byte { Type.js_name; ocaml_type; ocaml_name } =
  let extra_args = if multi_byte then "-> little_endian:bool " else "" in
  Printf.printf
    {|val get_%s : t -> byte_offset:int %s-> %s [@@js.call "get%s"]|}
    ocaml_name
    extra_args
    ocaml_type
    js_name;
  print_newline ();
  Printf.printf
    {|val set_%s : t -> byte_offset:int -> value:%s %s-> unit [@@js.call "set%s"]|}
    ocaml_name
    ocaml_type
    extra_args
    js_name;
  print_newline ()
;;

let buffer_setters () =
  with_formatting ~f:(fun () ->
    List.iter Type.single_byte ~f:(buffer_setter ~multi_byte:false);
    List.iter Type.multi_byte ~f:(buffer_setter ~multi_byte:true))
;;
