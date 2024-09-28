module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type Dict = sig
  type key
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : 'a t -> key -> 'a -> 'a t
  val length : 'a t -> int
  val of_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list
  val map : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold_left : ('acc -> key * 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val union : 'a t -> 'a t -> 'a t
end

module Make (Ord : OrderedType) : Dict with type key = Ord.t = struct
  type key = Ord.t
  type 'a t = Empty | Node of { l : 'a t; k : key; v : 'a; r : 'a t }

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

  let rec add_preserve t key value =
    match t with
    | Empty -> Node { l = Empty; k = key; v = value; r = Empty }
    | Node t when Ord.compare t.k key == -1 ->
        Node { l = add_preserve t.l key value; k = t.k; v = t.v; r = t.r }
    | Node t when Ord.compare t.k key == 1 ->
        Node { l = t.l; k = t.k; v = t.v; r = add_preserve t.r key value }
    | Node t when Ord.compare t.k key == 0 -> Node t
    | _ -> Empty

  let rec length t =
    match t with Node n -> 1 + length n.l + length n.r | _ -> 0

  let rec tail_of_list lst acc =
    match lst with
    | [] -> acc
    | (key, value) :: tl -> tail_of_list tl (add acc key value)

  let of_list lst = tail_of_list lst Empty

  let rec acc_to_list t acc =
    match t with
    | Empty -> acc
    | Node n -> acc_to_list n.l [] @ acc @ [ (n.k, n.v) ] @ acc_to_list n.r []

  let to_list t = acc_to_list t [] |> List.rev

  let rec map f t =
    match t with
    | Node n -> Node { l = map f n.l; r = map f n.r; k = n.k; v = f n.k n.v }
    | Empty -> Empty

  let rec fold_left f acc t =
    match t with
    | Node { l; k; v; r } -> fold_left f (f (fold_left f acc l) (k, v)) r
    | Empty -> acc

  let rec union d1 d2 =
    match d2 with
    | Node { l; r; k; v } -> union (union (add_preserve d1 k v) l) r
    | Empty -> d1
end
