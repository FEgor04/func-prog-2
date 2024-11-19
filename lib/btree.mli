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

  val add : key -> 'a -> 'a t -> 'a t
  val has : key -> 'a t -> bool
  val of_list : (key * 'a) list -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Returns dict with function applied to all of its values *)

  val to_list : 'a t -> (key * 'a) list
  val merge : 'a t -> 'a t -> 'a t
  val ( >>= ) : 'a t -> 'a t -> 'a t
  val fold_left : ('acc -> key * 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val fold_right : (key * 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
  val height : 'a t -> int
  val remove : key -> 'a t -> 'a t
end

module Make (Ord : OrderedType) (_ : BTreeConfig) : Dict with type key = Ord.t
