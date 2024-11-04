module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type BTreeConfig = sig
  val t : int
end

module type Dict = sig
  type key
  type 'a t

  val empty : 'a t
  (** Empty dictionary *)

  val singleton : key -> 'a -> 'a t
  (** Dictionary with only one key *)

  val is_empty : 'a t -> bool
  (** Checks if given dictionary is empty *)

  val find : key -> 'a t -> 'a option
  (** Returns value associated with given key *)
end

module Make (Ord : OrderedType) (_ : BTreeConfig) : Dict with type key = Ord.t
