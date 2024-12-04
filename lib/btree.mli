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
  (** [empty] is an empty dictionary *)

  val singleton : key -> 'a -> 'a t
  (** [singleton k v] returns a dictionary with given [key] and [value] *)

  val is_empty : 'a t -> bool
  (** [is_empty t] returns [true] if [t] is [Empty] and [false] otherwise *)

  val find : key -> 'a t -> 'a option
  (** [find k t] returns [Some v] if [t] contains a [key; v] entry and [None]
      otherwise *)

  val add : key -> 'a -> 'a t -> 'a t
  (** [add k v t] returns a new dictionary [t'] that contains all entries from
      [t] plus [k; v]. If [t] already has key [k], add wil overwrite it *)

  val has : key -> 'a t -> bool
  (** [has k t] returns [true] if [t] contains key [k] and [false] otherwise *)

  val of_list : (key * 'a) list -> 'a t
  (** [of_list lst] returns a new dictionary [t] that contains all entries from
      [lst] *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f t] returns a new dict [t'] with [f] applied to all of its values *)

  val to_list : 'a t -> (key * 'a) list
  (** [to_list t] returns a list [lst] that contains all keys and values from
      [t] *)

  val merge : 'a t -> 'a t -> 'a t
  (** [merge a b] returns a new dict [c] that contains all keys and values from
      [a] and [b]. If some of those keys intersect, [c] will contains values
      from [a] *)

  val ( >>= ) : 'a t -> 'a t -> 'a t
  (** [a >>= b] is [merge a b] *)

  val fold_left : ('acc -> key * 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  (** [fold_left f acc t] returns a result of function [f] sequentially applied
      to all elements of [t] in an ascending order *)

  val fold_right : (key * 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
  (** [fold_right f acc t] returns a result of function [f] sequentially applied
      to all elements of [t] in a descending order *)

  val height : 'a t -> int
  (** [height t] returns a height of BTree. It is only used to test BTree's
      internal structure *)

  val remove : key -> 'a t -> 'a t
  (** [remove k t] returns a new dict [t'] that does not contain key [k] *)

  val filter : (key * 'a -> bool) -> 'a t -> 'a t
  (** [filter f t] returns a new dict [t'] so that all of its entries satisfies
      predicate [f] *)

  val ( ^-^ ) : 'a t -> 'a t -> bool
  (** [a ^-^ b] is true if all keys and values of [a] equal [b] *)
end

module Make (Ord : OrderedType) (_ : BTreeConfig) : Dict with type key = Ord.t
