module BTreeConfig = struct
  let t = 10
end

module IntDict = Btree.Make (Int) (BTreeConfig)
open IntDict

let count = 1000
let generate_assoc_list = QCheck.(list (pair int int))

let add_from_list_has_key =
  QCheck.Test.make ~count ~name:"add_from_list_has_key" generate_assoc_list
    (fun lst ->
      let dict = lst |> IntDict.of_list in
      let has_key (item, _) = dict |> IntDict.has item in
      let has_all = lst |> List.map has_key |> List.fold_left ( && ) true in
      has_all)

let of_list_to_list_is_identity =
  QCheck.Test.make ~count ~name:"of_list_to_list_is_identity"
    generate_assoc_list (fun lst ->
      let sort_by_key (k, _) (k', _) = Int.compare k k' in
      let lst_sorted = List.sort_uniq sort_by_key lst in
      let dict = lst |> IntDict.of_list in
      let lst1 = dict |> IntDict.to_list in
      lst_sorted = lst1)

let merge_empty_is_neutral =
  QCheck.Test.make ~count ~name:"dict >>= Empty = Empty >>= dict = dict"
    generate_assoc_list (fun l1 ->
      let d1 = l1 |> IntDict.of_list in
      let d2 = IntDict.empty in
      let d = d1 >>= d2 in
      let d' = d1 >>= d2 in
      (d ^-^ d') && (d' ^-^ d1))

let merge_is_associative =
  QCheck.Test.make ~count ~name:"(x >>= y) >>= z = x >>= (y >>= z)"
    QCheck.(triple generate_assoc_list generate_assoc_list generate_assoc_list)
    (fun (l1_raw, l2_raw, l3_raw) ->
      let d1 = IntDict.of_list l1_raw in
      let d2 = IntDict.of_list l2_raw in
      let d3 = IntDict.of_list l3_raw in
      let r1 = d1 >>= d2 >>= d3 in
      let r2 = d1 >>= (d2 >>= d3) in
      r1 ^-^ r2)

let fold_left_is_to_list =
  QCheck.Test.make ~count ~name:"fold with concat is the same as to_list"
    generate_assoc_list (fun lst ->
      let d = lst |> IntDict.of_list in
      let l = d |> IntDict.to_list |> List.rev in
      let l' = IntDict.fold_left (fun acc kv -> kv :: acc) [] d in
      l = l')

let fold_right_is_to_list_rev =
  QCheck.Test.make ~count ~name:"fold_right is to list" generate_assoc_list
    (fun lst ->
      let d = IntDict.of_list lst in
      let l = d |> IntDict.to_list in
      let l' = IntDict.fold_right (fun kv acc -> kv :: acc) [] d in
      l = l')

let no_find_after_remove =
  QCheck.Test.make ~count ~name:"can't find key after remove"
    QCheck.(pair generate_assoc_list int)
    (fun (lst, elem) ->
      let d = IntDict.of_list lst in
      QCheck.assume (IntDict.has elem d = true);
      let d' = IntDict.remove elem d in
      let has = IntDict.has elem d' in
      has = false)

let filter_false =
  QCheck.Test.make ~count ~name:"filter(false) is always empty"
    generate_assoc_list (fun lst ->
      let d = IntDict.of_list lst in
      let d_filtered = d |> IntDict.filter (fun _ -> false) in
      IntDict.is_empty d_filtered)

let filter_true =
  QCheck.Test.make ~count ~name:"filter(true) is the same dict"
    generate_assoc_list (fun lst ->
      let d = IntDict.of_list lst in
      let d_filtered = d |> IntDict.filter (fun _ -> true) in
      d_filtered ^-^ d)

let equals_for_same_lists =
  QCheck.Test.make ~count ~name:"l |> of_dist = l |> of_dict"
    generate_assoc_list (fun l_raw ->
      let d1 = IntDict.of_list l_raw in
      let d2 = IntDict.of_list l_raw in
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
