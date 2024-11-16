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

  (** Splits the child node at index `i` of the given parent node. 
    Updates the parent node with the new split structure. *)
  let split_in_half = function
    | Empty -> None (* Cannot split an empty node *)
    | Node { keys; children } ->
        let n = List.length keys in
        if n < 1 then None (* Cannot split a node without keys *)
        else
          let mid_idx = n / 2 in
          (* Split keys into left and right around the middle key *)
          let[@warning "-partial-match"] left_keys, mid_key :: right_keys =
            Utils.list_split_idx mid_idx keys
          in
          (* Split children into left and right around the middle index *)
          let left_children, right_children =
            if List.is_empty children then ([], [])
            else Utils.list_split_idx (mid_idx + 1) children
          in
          Some
            ( (left_keys, left_children),
              (* Left half *)
              (right_keys, right_children),
              (* Right half *)
              mid_key (* Middle key *) )

  let split_child i = function
    | Empty -> failwith "Cannot split a child of an empty node"
    | Node { children; keys } -> (
        let child = List.nth children i in
        match split_in_half child with
        | None -> failwith "Cannot split an empty child"
        | Some
            ((left_keys, left_children), (right_keys, right_children), mid_key)
          ->
            (* Create new left and right nodes from the split *)
            let left_node =
              Node { children = left_children; keys = left_keys }
            in
            let right_node =
              Node { children = right_children; keys = right_keys }
            in

            (* Insert the middle key into the parent's keys at index `i` *)
            let new_keys = Utils.append_at keys [ mid_key ] i in

            (* Replace the child at index `i` with the new left and right nodes *)
            let new_children =
              Utils.append_at children [ left_node; right_node ] i
              |> List.filteri (fun idx _ -> idx <> i + 2)
              (* Remove the original child node *)
            in
            Node { children = new_children; keys = new_keys })

  let rec add_nonfull key value = function
    | Empty ->
        singleton key
          value (* Base case: if empty, return a new single-node tree *)
    | Node { children; keys } -> (
        let kv_compare (k1, _) (k2, _) = Ord.compare k1 k2 in
        if List.is_empty children then
          (* If the node is a leaf, add the key-value pair to the sorted list of keys *)
          let updated_keys = Utils.add_to_sorted keys (key, value) kv_compare in
          Node { children; keys = updated_keys }
        else
          (* If the node is internal, find the appropriate child to recurse into *)
          let child_idx = Utils.lower_bound keys (key, value) kv_compare in
          let child = List.nth children child_idx in
          match child with
          | Empty -> failwith "Unexpected Empty child node in non-empty BTree"
          | Node { children = _c_keys; keys = c_values } ->
              if List.length c_values = (2 * Config.t) - 1 then
                (* If the child is full, split it and adjust the current node *)
                let current = split_child child_idx (Node { children; keys }) in
                add_nonfull key value
                  current (* Retry insertion on the adjusted tree *)
              else
                (* If the child is not full, recursively insert into the child *)
                let updated_child = add_nonfull key value child in
                Node
                  {
                    children = Utils.update_at children child_idx updated_child;
                    keys;
                  })

  (** Adds a (key, value) pair to the BTree. Handles the case where the root is full by splitting it first. *)
  let add key value tree =
    match tree with
    | Empty -> singleton key value (* Base case: if empty, create a new tree *)
    | Node { children = _; keys } ->
        if List.length keys = (2 * Config.t) - 1 then
          (* If the root is full, split it and create a new root *)
          let (lk, lc), (rk, rc), mid = split_in_half tree |> Option.get in
          let left = Node { children = lc; keys = lk } in
          let right = Node { children = rc; keys = rk } in
          let new_root = Node { children = [ left; right ]; keys = [ mid ] } in
          add_nonfull key value new_root (* Retry insertion on the new root *)
        else
          (* If the root is not full, insert directly *)
          add_nonfull key value tree

  let of_list lst =
    let initial = empty in
    let add_to_acc acc (k, v) = acc |> add k v in
    lst |> List.fold_left add_to_acc initial

  let rec map f = function
    | Empty -> Empty
    | Node { children; keys } ->
        let map_key (k, v) = (k, f v) in
        let keys_mapped = keys |> List.map map_key in
        let map_child c = map f c in
        let children_mapped = children |> List.map map_child in
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
        let prev_res = List.fold_left fold_key_child acc keys_enumerated in
        let last_child = Utils.list_last children |> Option.get in
        let res = fold_left f prev_res last_child in
        res

  let rec fold_right f acc = function
    | Empty -> acc
    | Node { children; keys } when List.is_empty children ->
        List.fold_right f keys acc
    | Node { children; keys } ->
        let enumerate i x = (i, x) in
        let keys_enumerated = List.mapi enumerate keys in
        let n_children = List.length children in
        let fold_key_child (i, key) acc =
          f key (fold_right f acc (List.nth children (n_children - i - 1)))
        in
        let prev_res = List.fold_right fold_key_child keys_enumerated acc in
        let first_child = List.nth children 0 in
        let res = fold_right f prev_res first_child in
        res

  let rec height = function
    | Empty -> 0
    | Node { children; keys = _ } when List.is_empty children -> 1
    | Node { children; keys = _ } ->
        let children_height = children |> List.map height in
        let max_height = List.fold_left max 0 children_height in
        max_height + 1
end
