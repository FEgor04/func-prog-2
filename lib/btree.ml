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
  val add : key -> 'a -> 'a t -> 'a t
  val has : key -> 'a t -> bool
  val of_list : (key * 'a) list -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val to_list : 'a t -> (key * 'a) list
  val merge : 'a t -> 'a t -> 'a t
  val ( @ ) : 'a t -> 'a t -> 'a t
  val fold_left : ('acc -> key * 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val fold_right : (key * 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
  val height : 'a t -> int
end

module Make (Ord : OrderedType) (Config : BTreeConfig) :
  Dict with type key = Ord.t = struct
  type key = Ord.t
  type 'a t = Empty | Node of { children : 'a t list; keys : (key * 'a) list }

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false
  let singleton k v = Node { children = []; keys = [ (k, v) ] }

  (** finds index of a first key in given node that is bigger than given key*)
  let find_child_index_by_key key = function
    | Empty -> None
    | Node { children; keys } -> (
        if List.is_empty children then None
        else
          (* find index of the first key that is bigger than given *)
          let is_desired_key (k, _) = Ord.compare k key = 1 in
          let desired_index = List.find_index is_desired_key keys in
          match desired_index with None -> Some 0 | Some x -> Some (x + 1))

  let node_key_by_idx idx = function
    | Empty -> None
    | Node { children = _; keys } -> Some (List.nth keys idx)

  (** Returns a child of given node in which `key` is supposed to be *)
  let find_children_by_key key = function
    | Empty -> None
    | Node n -> (
        let idx = find_child_index_by_key key (Node n) in
        match idx with Some idx -> Some (List.nth n.children idx) | _ -> None)

  let rec find key = function
    | Empty -> None
    | Node { children; keys } -> (
        let key_is_desired (k, _) = k = key in
        let key_in_keys = List.find_opt key_is_desired keys in
        match key_in_keys with
        | None -> (
            let next_child =
              find_children_by_key key (Node { children; keys })
            in
            match next_child with None -> None | Some child -> find key child)
        | Some (_, v) -> Some v)

  let has key t = t |> find key |> Option.is_some

  (** Splits given node child with given index on two and updates given node *)
  let split_child i = function
    | Empty -> Empty
    | Node { children; keys } ->
        let to_split = List.nth children i in
        let splitted_left = Node { children = []; keys = [] } in
        let splitted_right = Node { children = []; keys = [] } in
        let key_value = node_key_by_idx i to_split |> Option.get in
        let keys_updated = key_value :: keys in
        let sort_by_key (a, _) (b, _) = Ord.compare a b in
        let keys_sorted = List.sort_uniq sort_by_key keys_updated in
        let children_left, _, children_right =
          Utils.list_split_idx i children
        in
        let children =
          children_left @ [ splitted_left ] @ [ splitted_right ]
          @ children_right
        in
        Node { children; keys = keys_sorted }

  let node_keys_length = function
    | Empty -> 0
    | Node { keys; children = _ } -> List.length keys

  (** Adds given node to non-full node.
      Implementation is mostly copied from BTree conspect on neerc.ifmo.ru *)
  let rec add_nonfull key value = function
    | Empty -> singleton key value
    | Node { children; keys } ->
        let is_leaf = List.is_empty children in
        if is_leaf then
          let keys_updated = (key, value) :: keys in
          let compare_by_key (a, _) (b, _) = Ord.compare a b in
          let keys_sorted = keys_updated |> List.sort_uniq compare_by_key in
          Node { children = []; keys = keys_sorted }
        else
          let idx =
            find_child_index_by_key key (Node { children; keys }) |> Option.get
          in
          let child = List.nth children idx in
          let n = node_keys_length child in
          let split_child =
            if n == (2 * Config.t) - 1 then split_child idx child else child
          in
          add_nonfull key value split_child

  (** Adds given node to tree. If current node is full, it will split it. *)
  let add key value = function
    | Empty -> singleton key value
    | Node { children; keys } when List.length children = (2 * Config.t) - 1 ->
        let new_root = split_child 1 (Node { children; keys }) in
        add_nonfull key value new_root
    | Node { children; keys } -> add_nonfull key value (Node { children; keys })

  let of_list lst =
    let initial = empty in
    let add_to_acc acc (k, v) = acc |> add k v in
    lst |> List.fold_left add_to_acc initial

  let rec map f = function
    | Empty -> Empty
    | Node { children; keys } ->
        let map_key (k, v) = (k, f v) in
        let keys_mapped = keys |> List.map map_key in
        let map_children c = map f c in
        let children_mapped = children |> List.map map_children in
        Node { children = children_mapped; keys = keys_mapped }

  let rec to_list = function
    | Empty -> []
    | Node { children; keys } ->
        let children_list = children |> List.map to_list |> List.flatten in
        let cmp_by_key (a, _) (b, _) = Ord.compare a b in
        List.merge cmp_by_key children_list keys

  let merge t1 t2 =
    let t2_values = to_list t2 in
    let add_to_tree t (k, v) = add k v t in
    let result = List.fold_left add_to_tree t1 t2_values in
    result

  let ( @ ) = merge

  let rec fold_left f acc = function
    | Empty -> acc
    | Node { children; keys } when List.is_empty children ->
        List.fold_left f acc keys
    | Node { children; keys } ->
        let enumerate i x = (i, x) in
        let keys_enumerated = List.mapi enumerate keys in
        let fold_key_child acc (i, key) =
          f (fold_left f acc (List.nth children i)) key
        in
        List.fold_left fold_key_child acc keys_enumerated

  let rec fold_right f acc = function
    | Empty -> acc
    | Node { children; keys } when List.is_empty children ->
        List.fold_right f keys acc
    | Node { children; keys } ->
        let enumerate i x = (i, x) in
        let keys_enumerated = List.mapi enumerate keys in
        let fold_key_child (i, key) acc =
          f key (fold_right f acc (List.nth children i))
        in
        List.fold_right fold_key_child keys_enumerated acc

  let rec height = function
    | Empty -> 0
    | Node { children; keys = _ } ->
        let children_height = children |> List.map height in
        let max_height = List.fold_left max min_int children_height in
        max_height + 1
end
