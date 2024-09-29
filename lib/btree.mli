module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

(** Proof that [Dict] is a monoid.
  Consider M to be a set of all possible finite dictionaries with same key and value type.`
  Consider a [union] to be a binary operation on M.
  It is obviously associative: (x ∪ y) ∪ z = x ∪ (y ∪ z)
  A neutral element is [Dict.Empty]: x ∪ {} = x
    *)
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
  val map : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold_left : ('acc -> key * 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val union : 'a t -> 'a t -> 'a t
  val filter : (key * 'a -> bool) -> 'a t -> 'a t
end

module Make (Ord : OrderedType) : Dict with type key = Ord.t
