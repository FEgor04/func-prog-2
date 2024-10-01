module IntCompare = struct
  type t = int

  let compare = Int.compare
end

module IntDict = Btree.Make (IntCompare)
module IntSet = Set.Make (IntCompare)

let to_assoc l = l |> List.map (fun x -> (x, x))
let to_unique l = l |> IntSet.of_list |> IntSet.to_list

let of_list_to_list =
  QCheck.Test.make ~count:100 ~name:"of_list -> to_list returns the same"
    QCheck.(list int)
    (fun l ->
      l |> to_unique |> to_assoc |> IntDict.of_list |> IntDict.to_list
      = (l |> to_unique |> to_assoc))

let to_list_equals_fold_left =
  QCheck.Test.make ~count:100 ~name:"of_list -> to_list equals to fold_left arr"
    QCheck.(list int)
    (fun l_raw ->
      let l = l_raw |> to_unique in
      let dict = l |> to_assoc |> IntDict.of_list in
      let to_list = dict |> IntDict.to_list |> List.map (fun (_, v) -> v) in
      let fold_to_arr acc (_, v) = v :: acc in
      let fold = dict |> IntDict.fold_left fold_to_arr [] in
      to_list = fold)

let to_list_rev_equals_fold_right =
  QCheck.Test.make ~count:100
    ~name:"of_list -> to_list -> rev equals to fold_right arr"
    QCheck.(list int)
    (fun l_raw ->
      let l = l_raw |> to_unique in
      let dict = l |> to_assoc |> IntDict.of_list in
      let to_list =
        dict |> IntDict.to_list |> List.map (fun (_, v) -> v) |> List.rev
      in
      let fold_to_arr acc (_, v) = v :: acc in
      let fold = dict |> IntDict.fold_right fold_to_arr [] in
      to_list = fold)

let union_on_distinct_dicts =
  QCheck.Test.make ~count:100 ~name:"union contains keys from both dict"
    QCheck.(list int)
    (fun l_raw ->
      let l = l_raw |> to_unique in
      let l1, l2 = List.partition (fun x -> x < Int.max_int / 2) l in
      QCheck.assume (l1 <> []);
      QCheck.assume (l2 <> []);
      let d1 = l1 |> to_assoc |> IntDict.of_list in
      let d2 = l2 |> to_assoc |> IntDict.of_list in
      let expected = l |> to_assoc in
      let actual = IntDict.union d1 d2 |> IntDict.to_list in
      expected = actual)

let union_is_associative =
  QCheck.Test.make ~count:100 ~name:"union is associative"
    QCheck.(triple (list int) (list int) (list int))
    (fun (l1, l2, l3) ->
      QCheck.assume (l1 <> []);
      QCheck.assume (l2 <> []);
      QCheck.assume (l3 <> []);
      let x = l1 |> to_assoc |> IntDict.of_list in
      let y = l2 |> to_assoc |> IntDict.of_list in
      let z = l3 |> to_assoc |> IntDict.of_list in
      let first = IntDict.union (IntDict.union x y) z in
      let second = IntDict.union x (IntDict.union y z) in
      let first_list = first |> IntDict.to_list in
      let second_list = second |> IntDict.to_list in
      first_list = second_list)

let union_neutral =
  QCheck.Test.make ~count:100 ~name:"union with Empty is neutral"
    QCheck.(list int)
    (fun l ->
      let d = l |> to_assoc |> IntDict.of_list in
      let d1 = IntDict.union d IntDict.empty in
      let d2 = IntDict.union IntDict.empty d in
      let first_list = d |> IntDict.to_list in
      let second_list = d1 |> IntDict.to_list in
      let third_list = d2 |> IntDict.to_list in
      first_list = second_list && second_list = third_list)

let filter_false =
  QCheck.Test.make ~count:100 ~name:"filter with false is empty list"
    QCheck.(list int)
    (fun l_raw ->
      let l = l_raw |> to_unique in
      let d = l |> to_assoc |> IntDict.of_list in
      let expected = [] in
      let actual = d |> IntDict.filter (fun _ -> false) |> IntDict.to_list in
      expected = actual)

let filter_true =
  QCheck.Test.make ~count:100 ~name:"filter with true is same dict"
    QCheck.(list int)
    (fun l_raw ->
      let l = l_raw |> to_unique in
      let d = l |> to_assoc |> IntDict.of_list in
      let expected = l |> to_assoc in
      let actual = d |> IntDict.filter (fun _ -> true) |> IntDict.to_list in
      expected = actual)

let () =
  let list_interop_suite =
    List.map QCheck_alcotest.to_alcotest
      [
        of_list_to_list; to_list_equals_fold_left; to_list_rev_equals_fold_right;
      ]
  in
  let monoid_properties_suite =
    List.map QCheck_alcotest.to_alcotest
      [ union_on_distinct_dicts; union_is_associative; union_neutral ]
  in
  let filter_suite =
    List.map QCheck_alcotest.to_alcotest [ filter_false; filter_true ]
  in
  Alcotest.run "Btree property tests"
    [
      ("monoid properties", monoid_properties_suite);
      ("list interop", list_interop_suite);
      ("filter function", filter_suite);
    ]
