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
    (lst |> List.map (fun (k, v) -> (k, v*v)))
    (lst |> IntDict.of_list |> IntDict.map (fun _ v -> v * v) |> IntDict.to_list)

let () =
  let open Alcotest in
  run "Btree test"
    [
      ( "suite",
        [
          test_case "Test" `Quick of_list_length;
          test_case "Test to_list" `Quick to_list_equals;
          test_case "to_list returns sorted" `Quick to_list_sorted;
          test_case "map with square" `Quick map_to_list;
        ] );
    ]
