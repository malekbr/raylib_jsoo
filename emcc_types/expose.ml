open! Base

module Type_expose = struct
  type 'a t =
    { module_name : string
    ; typ : 'a
    }
end

type t =
  | Struct : _ Types.Structure.Description.t Type_expose.t -> t
  | Function : _ Functions.t -> t

module Registry = struct
  type nonrec t = t Stack.t

  let create () = Stack.create ()
end

let structure (registry : Registry.t) ~module_name typ =
  Struct { module_name; typ } |> Stack.push registry
;;

let func (registry : Registry.t) f = Function f |> Stack.push registry
