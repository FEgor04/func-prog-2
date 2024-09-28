module IntCompare = struct
  type t = int

  let compare = Int.compare
end

module IntDict = Btree.Make (IntCompare)

let lst = [ (1, 1); (2, 2); (3, 3) ]

let of_list_length () =
  Alcotest.(check int) "same length" 3 (lst |> IntDict.of_list |> IntDict.length)

let to_list_equals () =
  Alcotest.(check (list (pair int int)))
    "same length" lst
    (lst |> IntDict.of_list |> IntDict.to_list)

let lst_unsorted = [ (1, 1); (5, 5); (3, 3); (10, 10); (4, 4) ]

let to_list_sorted () =
  Alcotest.(check (list (pair int int)))
    "should be equal"
    (lst_unsorted |> List.sort (fun (a, _) (b, _) -> Int.compare a b))
    (lst_unsorted |> IntDict.of_list |> IntDict.to_list)

let map_to_list () =
  Alcotest.(check (list (pair int int)))
    "should be equal"
    (lst |> List.map (fun (k, v) -> (k, v * v)))
    (lst |> IntDict.of_list |> IntDict.map (fun _ v -> v * v) |> IntDict.to_list)

let fold_left_sum () =
  Alcotest.(check int)
    "should be equal"
    (lst |> List.fold_left (fun acc (_, b) -> acc + b) 0)
    (lst |> IntDict.of_list |> IntDict.fold_left (fun acc (_, b) -> acc + b) 0)

let fold_left_to_list () =
  let lst = lst_unsorted in
  let expected = lst |> List.map (fun (_, v) -> v) |> List.sort Int.compare in
  let actual =
    lst |> IntDict.of_list |> IntDict.fold_left (fun acc (_, b) -> b :: acc) []
  in
  Alcotest.(check (list int)) "should be equal" expected actual

let union_distinct () =
  let to_assoc a = (a, a) in
  let l1 = [ 1; 2; 3; 4 ] |> List.map to_assoc in
  let l2 = [ 5; 6; 7; 8 ] |> List.map to_assoc in
  let expected = [ 1; 2; 3; 4; 5; 6; 7; 8 ] |> List.map to_assoc in
  let actual =
    IntDict.union (IntDict.of_list l1) (IntDict.of_list l2) |> IntDict.to_list
  in
  Alcotest.(check (list (pair int int))) "should be equal" expected actual

let union_intersection () =
  let to_assoc a = (a, a) in
  let l1 = [ 1; 2; 3; 4 ] |> List.map to_assoc in
  let l2 = [ 3; 4; 5; 6 ] |> List.map to_assoc in
  let expected = [ 1; 2; 3; 4; 5; 6 ] |> List.map to_assoc in
  let actual =
    IntDict.union (IntDict.of_list l1) (IntDict.of_list l2) |> IntDict.to_list
  in
  Alcotest.(check (list (pair int int))) "should be equal" expected actual

let union_intersection_different () =
  let to_assoc1 a = (a, a) in
  let to_assoc2 a = (a, 2 * a) in
  let l1 = [ 1; 2; 3; 4 ] |> List.map to_assoc1 in
  let l2 = [ 3; 4; 5; 6 ] |> List.map to_assoc2 in
  let expected =
    ([ 1; 2; 3; 4 ] |> List.map to_assoc1) @ ([ 5; 6 ] |> List.map to_assoc2)
  in
  let actual =
    IntDict.union (IntDict.of_list l1) (IntDict.of_list l2) |> IntDict.to_list
  in
  Alcotest.(check (list (pair int int))) "should be equal" expected actual

let () =
  let open Alcotest in
  run "Btree test"
    [
      ( "suite",
        [
          test_case "add + length test" `Quick of_list_length;
          test_case "to_list equals to expected" `Quick to_list_equals;
          test_case "to_list returns sorted" `Quick to_list_sorted;
          test_case "map with square" `Quick map_to_list;
          test_case "fold sum" `Quick fold_left_sum;
          test_case "fold to list" `Quick fold_left_to_list;
          test_case "union of distinct ints" `Quick union_distinct;
          test_case "union of intersection with same values" `Quick
            union_intersection;
          test_case "union of intersection with different values" `Quick
            union_intersection_different;
        ] );
    ]
