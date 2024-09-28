module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type Dict = sig
  type key
  type value
  type t

  val empty : t
  (** Empty dictionary **)

  val is_empty : t -> bool
  (** Checks if given dictionary is empty **)

  val add : t -> key -> value -> t
  (** Adds new key=value pair to given dictionary **)

  val length : t -> int
  (** Returns number of elemnst in given dictionary *)
end

module Make (Ord : OrderedType) : Dict with type key = Ord.t
