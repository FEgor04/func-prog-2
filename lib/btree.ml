module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type Dict = sig
  type key
  type value
  type t

  val empty : t
  val is_empty : t -> bool
  val add : t -> key -> value -> t
  val length : t -> int
end

module Make (Ord : OrderedType) : Dict with type key = Ord.t = struct
  type key = Ord.t
  type value
  type t = Empty | Node of { l : t; k : key; v : value; r : t }

  let empty = Empty
  let is_empty t = match t with Empty -> true | _ -> false

  let rec add t key value =
    match t with
    | Empty -> Node { l = Empty; k = key; v = value; r = Empty }
    | Node t when Ord.compare t.k key == -1 ->
        Node { l = add t.l key value; k = t.k; v = t.v; r = t.r }
    | Node t when Ord.compare t.k key == 1 ->
        Node { l = t.l; k = t.k; v = t.v; r = add t.r key value }
    | Node t when Ord.compare t.k key == 0 ->
        Node { l = t.l; k = t.k; v = value; r = t.r }
    | _ -> Empty

  let rec length t =
    match t with Node n -> 1 + length n.l + length n.r | _ -> 0
end
