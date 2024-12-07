module IntCompare = struct
  type t = int

  let compare = Int.compare
end

module BTreeConfig = struct
  let t = 10
end

module IntDict = Btree.Make (IntCompare) (BTreeConfig)

let to_assoc x = (x, x)
let to_assoc_lst = List.map to_assoc
let raw_to_sorted_assoc lst = lst |> List.sort_uniq Int.compare |> to_assoc_lst
let raw_to_dict lst = lst |> raw_to_sorted_assoc |> IntDict.of_list
let count = 1000

let add_from_list_has_key =
  QCheck.Test.make ~count ~name:"add_from_list_has_key"
    QCheck.(list int)
    (fun lst_raw ->
      let lst = lst_raw |> List.sort_uniq Int.compare in
      let lst_assoc = lst |> to_assoc_lst in
      let dict = lst_assoc |> IntDict.of_list in
      let has_key item = dict |> IntDict.has item in
      let has_all = lst |> List.map has_key |> List.fold_left ( && ) true in
      has_all)

let of_list_to_list_is_identity =
  QCheck.Test.make ~count ~name:"of_list_to_list_is_identity"
    QCheck.(list int)
    (fun lst_raw ->
      let lst = lst_raw |> List.sort_uniq Int.compare in
      let lst_assoc = lst |> to_assoc_lst in
      let dict = lst_assoc |> IntDict.of_list in
      let lst1 = dict |> IntDict.to_list in
      lst_assoc = lst1)

let merge_empty_is_neutral =
  QCheck.Test.make ~count ~name:"dict >>= Empty = Empty >>= dict = dict"
    QCheck.(list int)
    (fun l1_raw ->
      let open IntDict in
      let d1 = l1_raw |> raw_to_sorted_assoc |> IntDict.of_list in
      let d2 = IntDict.empty in
      let d = d1 >>= d2 in
      let d' = d1 >>= d2 in
      let d1_list = IntDict.to_list d1 in
      let d_list = IntDict.to_list d in
      let d'_list = IntDict.to_list d' in
      d_list = d1_list && d1_list = d'_list)

let merge_is_associative =
  QCheck.Test.make ~count ~name:"(x >>= y) >>= z = x >>= (y >>= z)"
    QCheck.(triple (list int) (list int) (list int))
    (fun (l1_raw, l2_raw, l3_raw) ->
      let open IntDict in
      let d1 = raw_to_dict l1_raw in
      let d2 = raw_to_dict l2_raw in
      let d3 = raw_to_dict l3_raw in
      let r1 = d1 >>= d2 >>= d3 in
      let r2 = d1 >>= (d2 >>= d3) in
      let r1_lst = r1 |> IntDict.to_list in
      let r2_lst = r2 |> IntDict.to_list in
      r1_lst = r2_lst)

let fold_left_is_to_list =
  QCheck.Test.make ~count ~name:"fold with concat is the same as to_list"
    QCheck.(list int)
    (fun l_raw ->
      let d = raw_to_dict l_raw in
      let l = d |> IntDict.to_list |> List.rev in
      let l' = IntDict.fold_left (fun acc kv -> kv :: acc) [] d in
      l = l')

let fold_right_is_to_list_rev =
  QCheck.Test.make ~count ~name:"fold_right is to list"
    QCheck.(list int)
    (fun l_raw ->
      let d = raw_to_dict l_raw in
      let l = d |> IntDict.to_list in
      let l' = IntDict.fold_right (fun kv acc -> kv :: acc) [] d in
      l = l')

let no_find_after_remove =
  QCheck.Test.make ~count ~name:"can't find key after remove"
    QCheck.(pair (list int) int)
    (fun (l_raw, elem) ->
      QCheck.assume (List.find_opt (fun x -> x = elem) l_raw |> Option.is_some);
      let d = raw_to_dict l_raw in
      let d' = IntDict.remove elem d in
      let has = IntDict.has elem d' in
      has = false)

let filter_false =
  QCheck.Test.make ~count ~name:"filter(false) is always empty"
    QCheck.(list int)
    (fun l_raw ->
      let d = raw_to_dict l_raw in
      let d_filtered = d |> IntDict.filter (fun _ -> false) in
      IntDict.is_empty d_filtered)

let filter_true =
  QCheck.Test.make ~count ~name:"filter(true) is the same dict"
    QCheck.(list int)
    (fun l_raw ->
      let d = raw_to_dict l_raw in
      let d_filtered = d |> IntDict.filter (fun _ -> true) in
      let d_list = d |> IntDict.to_list in
      let d_filtered_list = d_filtered |> IntDict.to_list in
      d_filtered_list = d_list)

let equals_for_same_lists =
  QCheck.Test.make ~count ~name:"l |> of_dist = l |> of_dict"
    QCheck.(list int)
    (fun l_raw ->
      let open IntDict in
      let d1 = raw_to_dict l_raw in
      let d2 = raw_to_dict l_raw in
      d1 ^-^ d2)

let () =
  let add_suite =
    List.map QCheck_alcotest.to_alcotest [ add_from_list_has_key ]
  in
  let of_list_to_list =
    List.map QCheck_alcotest.to_alcotest [ of_list_to_list_is_identity ]
  in
  let monoid_properties =
    List.map QCheck_alcotest.to_alcotest
      [ merge_empty_is_neutral; merge_is_associative ]
  in
  let fold_suite =
    List.map QCheck_alcotest.to_alcotest
      [ fold_left_is_to_list; fold_right_is_to_list_rev ]
  in
  let remove_suite =
    List.map QCheck_alcotest.to_alcotest [ no_find_after_remove ]
  in
  let filter_suite =
    List.map QCheck_alcotest.to_alcotest [ filter_false; filter_true ]
  in
  let equals_suite =
    List.map QCheck_alcotest.to_alcotest [ equals_for_same_lists ]
  in
  Alcotest.run "quickcheck"
    [
      ("add_suite", add_suite);
      ("of_list_to_list", of_list_to_list);
      ("monoid_properties", monoid_properties);
      ("fold", fold_suite);
      ("remove", remove_suite);
      ("filter", filter_suite);
      ("equals", equals_suite);
    ]
