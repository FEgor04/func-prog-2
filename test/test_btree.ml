module IntCompare = struct
  type t = int

  let compare = Int.compare
end

module BTreeConfig = struct
  let t = 10
end

module IntDict = Btree.Make (IntCompare) (BTreeConfig)

let to_assoc x = (x, x)
let to_assoc_list = List.map to_assoc

let test_find_empty () =
  Alcotest.(check bool)
    "empty dict is empty" true
    (IntDict.empty |> IntDict.find 5 |> Option.is_none)

let test_find_singleton_key_exists () =
  Alcotest.(check int)
    "singleton dict has given key" 10
    (IntDict.singleton 10 10 |> IntDict.find 10 |> Option.get)

let test_find_singleton_key_not_exists () =
  Alcotest.(check bool)
    "singleton dict has given key" true
    (IntDict.singleton 10 10 |> IntDict.find 5 |> Option.is_none)

let test_add_find () =
  Alcotest.(check bool)
    "add two keys sequentially and find them" true
    (let dict = IntDict.empty |> IntDict.add 1 1 |> IntDict.add 2 2 in
     let result = IntDict.has 1 dict && IntDict.has 2 dict in
     result)

let test_of_list () =
  Alcotest.(check bool)
    "of small list" true
    (let list = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
     let assoc = list |> List.map to_assoc in
     let dict = assoc |> IntDict.of_list in
     let has_key key = dict |> IntDict.has key in
     let has_keys = list |> List.map has_key in
     let has_all = has_keys |> List.fold_left ( && ) true in
     has_all)

let test_map_empty () =
  Alcotest.(check bool)
    "of small list" true
    (let dict = IntDict.empty in
     let square x = x * x in
     let dict_mapped = dict |> IntDict.map square in
     IntDict.is_empty dict_mapped)

let test_map_list () =
  Alcotest.(check bool)
    "map small list" true
    (let list = [ 1; 2; 3; 4 ] in
     let dict = list |> to_assoc_list |> IntDict.of_list in
     let square x = x * x in
     let dict_mapped = dict |> IntDict.map square in
     let get_value key = IntDict.find key dict_mapped |> Option.get in
     let values = list |> List.map get_value in
     list |> List.map square = values)

let test_fold_sum () =
  Alcotest.(check int)
    "1 + 2 + 3 + 4 = " 10
    (let list = [ 1; 2; 3; 4 ] in
     let dict = list |> to_assoc_list |> IntDict.of_list in
     let sum acc (_, v) = acc + v in
     let actual = dict |> IntDict.fold_left sum 0 in
     actual)

let () =
  let open Alcotest in
  run "BTree"
    [
      ( "find",
        [
          test_case "empty dict" `Quick test_find_empty;
          test_case "singleton dict, key exists" `Quick
            test_find_singleton_key_exists;
          test_case "singleton dict, key does not exist" `Quick
            test_find_singleton_key_not_exists;
        ] );
      ("add", [ test_case "add two keys then find them" `Quick test_add_find ]);
      ("of_list", [ test_case "small list" `Quick test_of_list ]);
      ( "map",
        [
          test_case "empty" `Quick test_map_empty;
          test_case "small" `Quick test_map_list;
        ] );
      ("fold_left", [ test_case "sum" `Quick test_fold_sum ]);
    ]
