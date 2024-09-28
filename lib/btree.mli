module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type Dict = sig
  type key
  type 'a t

  val empty : 'a t
  (** Empty dictionary **)

  val is_empty : 'a t -> bool
  (** Checks if given dictionary is empty **)

  val add : 'a t -> key -> 'a -> 'a t
  (** Adds new key=value pair to given dictionary **)

  val length : 'a t -> int
  (** Returns number of elemnst in given dictionary *)

  val of_list : (key * 'a) list -> 'a t
  (** Creates a new tree from given list **)

  val to_list : 'a t -> (key * 'a) list
end

module Make (Ord : OrderedType) : Dict with type key = Ord.t
