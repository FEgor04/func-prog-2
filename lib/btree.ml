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
  val singleton : key -> 'a -> 'a t


  val is_empty : 'a t -> bool
  val find : key -> 'a t -> 'a option
end

module Make (Ord : OrderedType) (Config : BTreeConfig) :
  Dict with type key = Ord.t = struct
  type key = Ord.t
  type 'a t = Empty | Node of { children : 'a t list; keys : (key * 'a) list }

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false
  let singleton k v = Node { children = []; keys = [ (k, v) ] }

  let find _ = function
    | Empty -> None
    | Node{children=_; keys=_} -> None
  end
