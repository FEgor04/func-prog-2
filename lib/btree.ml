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
  val ( >>= ) : 'a t -> 'a t -> 'a t
  val fold_left : ('acc -> key * 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val fold_right : (key * 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
  val height : 'a t -> int
  val remove : key -> 'a t -> 'a t
  val filter : (key * 'a -> bool) -> 'a t -> 'a t
  val ( ^-^ ) : 'a t -> 'a t -> bool
end

module Make (Ord : OrderedType) (Config : BTreeConfig) :
  Dict with type key = Ord.t = struct
  type key = Ord.t
  type 'a t = Empty | Node of { children : 'a t list; keys : (key * 'a) list }

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false
  let singleton k v = Node { children = []; keys = [ (k, v) ] }

  let find_key_idx key keys =
    let keys_only = keys |> List.map (fun (x, _) -> x) in
    Utils.lower_bound keys_only key Ord.compare

  (** Searches for a key in the BTree and returns the associated value, if it
      exists. *)
  let rec find key = function
    | Empty -> None
    | Node { keys; children } ->
        (* Compare keys in the node using the provided comparator *)
        let idx = find_key_idx key keys in

        if idx < List.length keys then
          let k, v = List.nth keys idx in
          if Ord.compare key k = 0 then Some v (* Key found in this node *)
          else if
            (* Otherwise, descend into the appropriate child *)
            List.is_empty children
          then None
          else find key (List.nth children idx)
        else if
          (* Key not found in this node; check the last child if it exists *)
          List.is_empty children
        then None
        else find key (List.nth children idx)

  let has key t = t |> find key |> Option.is_some

  (** Splits the child node at index `i` of the given parent node. Updates the
      parent node with the new split structure. *)
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
              mid_key
              (* Middle key *) )

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

            let new_keys = Utils.append_at keys [ mid_key ] i in

            let new_children =
              Utils.append_at children [ left_node; right_node ] i
              |> List.filteri (fun idx _ -> idx <> i + 2)
            in
            Node { children = new_children; keys = new_keys })

  let rec add_nonfull key value = function
    | Empty -> singleton key value
    | Node { children; keys } -> (
        let kv_compare (k1, _) (k2, _) = Ord.compare k1 k2 in
        if List.is_empty children then
          let updated_keys = Utils.add_to_sorted keys (key, value) kv_compare in
          let sorted_keys = List.sort kv_compare updated_keys in
          Node { children; keys = sorted_keys }
        else
          let child_idx = Utils.lower_bound keys (key, value) kv_compare in
          let child = List.nth children child_idx in
          match child with
          | Empty -> failwith "Unexpected Empty child node in non-empty BTree"
          | Node { children = _c_keys; keys = c_values } ->
              if List.length c_values = (2 * Config.t) - 1 then
                let current = split_child child_idx (Node { children; keys }) in
                add_nonfull key value current
              else
                let updated_child = add_nonfull key value child in
                Node
                  {
                    children = Utils.replace_at child_idx updated_child children;
                    keys;
                  })

  (** Adds a (key, value) pair to the BTree. Handles the case where the root is
      full by splitting it first. *)
  let add key value tree =
    match tree with
    | Empty -> singleton key value
    | Node { children = _; keys } ->
        if List.length keys = (2 * Config.t) - 1 then
          let (lk, lc), (rk, rc), mid = split_in_half tree |> Option.get in
          let left = Node { children = lc; keys = lk } in
          let right = Node { children = rc; keys = rk } in
          let new_root = Node { children = [ left; right ]; keys = [ mid ] } in
          add_nonfull key value new_root
        else add_nonfull key value tree

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

  let ( >>= ) = merge

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
        let rec process acc children keys =
          match (children, keys) with
          | [], [] -> acc
          | child :: rest_children, key :: rest_keys ->
              let acc_after_child = fold_right f acc child in
              let acc_after_key = f key acc_after_child in
              process acc_after_key rest_children rest_keys
          | child :: _rest_children, [] -> fold_right f acc child
          | [], keys -> List.fold_right f keys acc
        in
        process acc (List.rev children) (List.rev keys)

  let rec height = function
    | Empty -> 0
    | Node { children; keys = _ } when List.is_empty children -> 1
    | Node { children; keys = _ } ->
        let children_height = children |> List.map height in
        let max_height = List.fold_left max min_int children_height in
        max_height + 1

  let remove_from_leaf idx = function
    | Empty -> failwith "Illegal remove_from_leaf call: node is empty"
    | Node { children = []; keys } ->
        let updated_keys = List.filteri (fun i _ -> i != idx) keys in
        Node { children = []; keys = updated_keys }
    | _ -> failwith "Illegal remove_from_leaf call: node is not a leaf"

  let rec get_rightmost_key = function
    | Empty -> None
    | Node { children = []; keys } -> Utils.list_last keys
    | Node { children; keys = _ } ->
        Utils.list_last children |> Option.get |> get_rightmost_key

  let rec get_leftmost_key = function
    | Empty -> None
    | Node { children = []; keys } -> Some (List.hd keys)
    | Node { children; keys = _ } -> List.hd children |> get_leftmost_key

  let remove_last_key = function
    | Empty -> (Empty, None, None)
    | Node { children = []; keys } ->
        let n = List.length keys in
        let last_key = List.nth keys (n - 1) in
        let updated_keys = List.filteri (fun i _ -> i < n - 1) keys in
        (Node { children = []; keys = updated_keys }, Some last_key, None)
    | Node { children; keys } ->
        let n = List.length keys in
        let last_key = List.nth keys (n - 1) in
        let last_child = List.nth children n in
        let updated_keys = Utils.remove_last keys in
        let updated_children = Utils.remove_last children in
        ( Node { children = updated_children; keys = updated_keys },
          Some last_key,
          Some last_child )

  let add_first_key_child key child = function
    | Empty -> failwith "illegal add_first_key_child call"
    | Node { children; keys } ->
        let children_updated =
          match child with Some x -> x :: children | None -> children
        in
        let keys_updated = key :: keys in
        Node { children = children_updated; keys = keys_updated }

  (** Borrows a key from C[idx-1] and inserts it into C[idx] *)
  let borrow_from_prev idx = function
    | Empty -> Empty
    | Node { children; keys } ->
        let child = List.nth children idx in
        let sibling = List.nth children (idx - 1) in
        let cur = List.nth keys (idx - 1) in
        let updated_sibling, k1, c1 = remove_last_key sibling in
        let updated_child = add_first_key_child cur c1 child in
        let updated_keys = Utils.replace_at (idx - 1) (Option.get k1) keys in
        let updated_children =
          children
          |> Utils.replace_at idx updated_child
          |> Utils.replace_at (idx - 1) updated_sibling
        in
        Node { children = updated_children; keys = updated_keys }

  let remove_first_key = function
    | Empty -> (Empty, None, None)
    | Node { children = []; keys } ->
        let first_key = List.hd keys in
        let updated_keys = List.tl keys in
        (Node { children = []; keys = updated_keys }, Some first_key, None)
    | Node { children; keys } ->
        let first_key = List.hd keys in
        let first_child = List.hd children in
        let updated_keys = List.tl keys in
        let updated_children = List.tl children in
        ( Node { children = updated_children; keys = updated_keys },
          Some first_key,
          Some first_child )

  let add_last_key_child key child = function
    | Empty -> failwith "illegal add_last_key_child call"
    | Node { children; keys } ->
        let children_updated =
          match child with Some x -> children @ [ x ] | None -> children
        in
        let keys_updated = keys @ [ key ] in
        Node { children = children_updated; keys = keys_updated }

  (** Borrows a key from C[idx+1] and inserts it into C[idx] *)
  let borrow_from_next idx = function
    | Empty -> Empty
    | Node { children; keys } ->
        let child = List.nth children idx in
        let sibling = List.nth children (idx + 1) in
        let cur = List.nth keys idx in
        let updated_sibling, k1, c1 = remove_first_key sibling in
        let updated_child = add_last_key_child cur c1 child in
        let updated_keys = Utils.replace_at idx (Option.get k1) keys in
        let updated_children =
          children
          |> Utils.replace_at idx updated_child
          |> Utils.replace_at (idx + 1) updated_sibling
        in
        Node { children = updated_children; keys = updated_keys }

  (** Merges children[idx] with children[idx+1] *)
  let merge_with_next idx = function
    | Empty -> failwith "Illegal merge call"
    | Node { children; keys } -> (
        let child = List.nth children idx in
        let sibling = List.nth children (idx + 1) in
        match (child, sibling) with
        | ( Node { children = c_children; keys = c_keys },
            Node { children = s_children; keys = s_keys } ) ->
            let child_keys = c_keys @ [ List.nth keys idx ] @ s_keys in
            let child_children = c_children @ s_children in
            let child = Node { children = child_children; keys = child_keys } in
            let keys_updated = keys |> Utils.remove_at idx in
            let children_updated =
              children
              |> Utils.remove_at (idx + 1)
              |> Utils.replace_at idx child
            in
            Node { children = children_updated; keys = keys_updated }
        | _, _ -> failwith "Illegal merge call")

  let node_n = function
    | Empty -> 0
    | Node { keys; children = _ } -> List.length keys

  let fill idx = function
    | Empty -> failwith "Illegal fill call"
    | Node { children; keys } -> (
        let n = List.length keys in
        let previous = List.nth_opt children (idx - 1) in
        let previous_n = Option.map node_n previous in
        let next = List.nth_opt children (idx + 1) in
        let next_n = Option.map node_n next in
        match (previous_n, next_n) with
        | Some prev_n, _ when prev_n >= Config.t ->
            borrow_from_prev idx (Node { children; keys })
        | _, Some next_n when next_n >= Config.t ->
            borrow_from_next idx (Node { children; keys })
        | _, _ when idx != n -> merge_with_next idx (Node { children; keys })
        | _, _ -> merge_with_next (idx - 1) (Node { children; keys }))

  let get_succ idx = function
    | Empty -> None
    | Node { children; keys = _ } ->
        let next = List.nth children (idx + 1) in
        get_leftmost_key next

  let get_pred idx = function
    | Empty -> None
    | Node { children; keys = _ } ->
        let prev = List.nth children (idx - 1) in
        get_rightmost_key prev

  let node_child idx = function
    | Empty -> None
    | Node { children; keys = _ } -> List.nth_opt children idx

  let rec remove key t =
    let remove_from_nonleaf idx = function
      | Empty -> Empty
      | Node { children; keys } -> (
          let k, _ = List.nth keys idx in
          let target = List.nth children idx in
          let next = List.nth children (idx + 1) in
          let target_n = node_n target in
          let next_n = node_n next in
          match (target_n, next_n) with
          | target_n, _ when target_n >= Config.t ->
              let pred = get_pred idx (Node { children; keys }) |> Option.get in
              let pred_k, _ = pred in
              let keys_updated = keys |> Utils.replace_at idx pred in
              let target_updated = remove pred_k target in
              let children_updated =
                children |> Utils.replace_at idx target_updated
              in
              Node { children = children_updated; keys = keys_updated }
          | _, next_n when next_n >= Config.t ->
              let succ = get_succ idx (Node { children; keys }) |> Option.get in
              let succ_k, _ = succ in
              let keys_updated = keys |> Utils.replace_at idx succ in
              let next_updated = remove succ_k next in
              let children_updated =
                children |> Utils.replace_at (idx + 1) next_updated
              in
              Node { children = children_updated; keys = keys_updated }
          | _, _ ->
              let merged = merge_with_next idx (Node { children; keys }) in
              remove k merged)
    in
    match t with
    | Empty -> Empty
    | Node { children; keys } -> (
        let idx = find_key_idx key keys in
        let n = List.length keys in
        let keys_idx = List.nth_opt keys idx in
        match keys_idx with
        | Some (k, _) when k = key && List.is_empty children ->
            remove_from_leaf idx (Node { children; keys })
        | Some (k, _) when k = key ->
            remove_from_nonleaf idx (Node { children; keys })
        | _ ->
            let flag = idx == n in
            let child_idx = List.nth children idx in
            let child_idx_n = node_n child_idx in
            let self_updated =
              if child_idx_n < Config.t then fill idx (Node { children; keys })
              else Node { children; keys }
            in
            let n_updated = node_n self_updated in
            let child_idx = if flag && idx > n_updated then idx - 1 else idx in
            let child = node_child child_idx self_updated |> Option.get in
            let child_updated = remove key child in
            let children_updated =
              children |> Utils.replace_at child_idx child_updated
            in
            Node { children = children_updated; keys })

  let filter f t =
    let filtered = t |> to_list |> List.filter f in
    of_list filtered

  let rec equals t1 t2 =
    match (t1, t2) with
    | Node { children = []; keys = k1 }, Node { children = []; keys = k2 } ->
        k1 = k2
    | Node { children = c1; keys = k1 }, Node { children = c2; keys = k2 }
      when k1 = k2 && List.length c1 = List.length c2 ->
        let equal_to_t2 i c = equals c (List.nth c2 i) in
        let children_equal = c1 |> List.mapi equal_to_t2 in
        children_equal |> List.fold_left ( && ) true
    | Empty, Empty -> true
    | _, _ -> false

  let ( ^-^ ) = equals
end
