open! Base

module Arguments = struct
  type 'a t =
    | Return : 'a Types.t -> 'a t
    | Argument : 'a Types.t * 'b t -> ('a -> 'b) t
end

type 'a t =
  { name : string
  ; arguments : 'a Arguments.t
  }

let returning a = Arguments.Return a
let ( @-> ) a b = Arguments.Argument (a, b)
let foreign name arguments = { name; arguments }
