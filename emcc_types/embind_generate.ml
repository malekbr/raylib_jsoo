open! Base

let generate_structure { Types.Structure.Description.name; fields } =
  let[@tail_mod_cons] rec field_values
    : type a b. (a, b) Types.Structure.Fields.t -> string list
    = function
    | [] -> []
    | field :: rest ->
      let field_name = Types.Structure.Field.name field in
      Printf.sprintf "field(%S, &%s::%s)" field_name name field_name :: field_values rest
  in
  Printf.sprintf "value_object<%s>(%S)" name name :: field_values fields
  |> String.concat ~sep:"."
  |> Printf.sprintf "%s;"
;;

let generate_function { Functions.name; _ } = Printf.sprintf "function(%S, %s);" name name

let generate (registry : Expose.Registry.t) =
  Stack.to_list registry
  |> List.rev_map ~f:(function
       | Expose.Struct { typ; _ } -> generate_structure typ
       | Function func -> generate_function func)
  |> String.concat ~sep:"\n  "
  |> Printf.sprintf
       {|#include <emscripten/bind.h>
using namespace emscripten;
EMSCRIPTEN_BINDINGS(emcc_types) {
  %s
}
|}
;;
