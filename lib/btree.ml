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

  let find_children_by_key key = function
    | Empty -> Empty
    | Node { children; keys } -> (
        if List.is_empty children then Empty
        else
          (* find index of the first key that is bigger than given *)
          let is_desired_key (k, _) = Ord.compare k key = 1 in
          let desired_index = List.find_index is_desired_key keys in
          match desired_index with
          | None -> List.hd children
          | Some idx -> List.nth children (idx + 1))

  let rec find key = function
    | Empty -> None
    | Node { children; keys } -> (
        let key_is_desired (k, _) = k = key in
        let key_in_keys = List.find_opt key_is_desired keys in
        match key_in_keys with
        | None ->
            let next_child =
              find_children_by_key key (Node { children; keys })
            in
            find key next_child
        | Some (_, v) -> Some v)
end
