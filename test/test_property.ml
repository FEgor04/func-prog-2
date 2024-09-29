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

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [ of_list_to_list; union_on_distinct_dicts ]
  in
  Alcotest.run "my test" [ ("suite", suite) ]
