open! Base

module Primitive = struct
  type 'a t =
    | Int64_t : int64 t
    | Float : float t
    | Char : char t
end

module Pointer : sig
  type !'a t [@@immediate]

  val of_address : int -> _ t
  val address : _ t -> int
end = struct
  type 'a t = { address : int } [@@unboxed]

  let of_address address = { address }
  let address { address } = address
end

module rec Type : sig
  type 'a t =
    | Primitive : 'a Primitive.t -> 'a t
    | Structure : ('a Structure.t, _) Structure.Description.t -> 'a Structure.t t
    | Pointer : 'a t -> 'a Pointer.t t
end =
  Type

and Structure : sig
  module Field : sig
    type (!'r, 'f) t

    val create : string -> 'f Type.t -> ('r, 'f) t
    val name : _ t -> string
    val typ : (_, 'f) t -> 'f Type.t
  end

  module Fields : sig
    type (!'r, 'creator) t =
      | [] : ('r, 'r) t
      | ( :: ) : ('r, 'f) Field.t * ('r, 'creator) t -> ('r, 'f -> 'creator) t
  end

  module Description : sig
    type (!'r, 'creator) t =
      { name : string
      ; fields : ('r, 'creator) Fields.t
      }
  end

  type 'witness t
end = struct
  module Field = struct
    type (!'r, 'f) t =
      { name : string
      ; typ : 'f Type.t
      }

    let create name typ = { name; typ }
    let name { name; _ } = name
    let typ { typ; _ } = typ
  end

  module Fields = struct
    type (!'r, 'creator) t =
      | [] : ('r, 'r) t
      | ( :: ) : ('r, 'f) Field.t * ('r, 'creator) t -> ('r, 'f -> 'creator) t
  end

  module Description = struct
    type (!'r, 'creator) t =
      { name : string
      ; fields : ('r, 'creator) Fields.t
      }
  end

  type 'witness t
end

include Type
