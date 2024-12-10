module BTreeConfig = struct
  let t = 10
end

module IntDict = Btree.Make (Int) (BTreeConfig)
module StringDict = Btree.Make (String) (BTreeConfig)
module IntToStringMapper = Mapper.Make (Int) (String) (BTreeConfig)

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

let test_add_conflict () =
  let new_value = 100 in
  Alcotest.(check int)
    "add key when it already exists" new_value
    (let dict = IntDict.empty |> IntDict.add 1 1 in
     let d' = dict |> IntDict.add 1 new_value in
     let result = d' |> IntDict.find 1 |> Option.get in
     result)

let test_of_list () =
  Alcotest.(check (list int))
    "of small list"
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]
    (let list = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
     let assoc = list |> List.map to_assoc in
     let dict = assoc |> IntDict.of_list in
     dict |> IntDict.to_list |> List.map (fun (x, _) -> x))

let test_map_empty () =
  Alcotest.(check bool)
    "of small list" true
    (let dict = IntDict.empty in
     let square x = x * x in
     let dict_mapped = dict |> IntDict.map square in
     IntDict.is_empty dict_mapped)

let test_map_list () =
  Alcotest.(check (list int))
    "map small list" [ 1; 4; 9; 16 ]
    (let list = [ 1; 2; 3; 4 ] in
     let dict = list |> to_assoc_list |> IntDict.of_list in
     let square x = x * x in
     let dict_mapped = dict |> IntDict.map square in
     dict_mapped |> IntDict.to_list |> List.map (fun (_, x) -> x))

let test_map_poly () =
  let input = [ 1; 2; 3; 4; 5; 6 ] in
  let expected = [ "1"; "2"; "3"; "4"; "5"; "6" ] in
  Alcotest.(check (list string))
    "map small list" expected
    (let dict = input |> to_assoc_list |> IntDict.of_list in
     let dict_mapped = dict |> IntDict.map string_of_int in
     dict_mapped |> IntDict.to_list |> List.map (fun (_, x) -> x))

let test_map_different_key_types () =
  let input = [ 1; 2; 3; 4; 5; 6 ] in
  let expected = [ "1"; "2"; "3"; "4"; "5"; "6" ] in
  Alcotest.(check (list string))
    "map (int, int) -> ((string; string)" expected
    (let dict = input |> to_assoc_list |> IntDict.of_list in
     let dict_mapped =
       IntToStringMapper.map
         (fun (k, v) -> (string_of_int k, string_of_int v))
         dict
     in
     dict_mapped |> StringDict.to_list |> List.map (fun (k, _) -> k))

let test_fold_sum () =
  Alcotest.(check int)
    "1 + 2 + 3 + 4 = " 10
    (let list = [ 1; 2; 3; 4 ] in
     let dict = list |> to_assoc_list |> IntDict.of_list in
     let sum acc (_, v) = acc + v in
     let actual = dict |> IntDict.fold_left sum 0 in
     actual)

let test_fold_right_list () =
  let lst = [ 0; -1; 1; 2; 3; 4 ] in
  let lst_assoc = lst |> List.sort Int.compare |> to_assoc_list in
  Alcotest.(check (list (pair int int)))
    "fold_right to list" lst_assoc
    (let list = lst in
     let dict = list |> to_assoc_list |> IntDict.of_list in
     let fold_right_concat kv acc = kv :: acc in
     let actual = dict |> IntDict.fold_right fold_right_concat [] in
     actual)

let test_remove_key () =
  let lst = [ 0; 1; 2; 3; 4 ] in
  Alcotest.(check (list int))
    "fold_right to list" [ 0; 2; 3; 4 ]
    (let dict = lst |> to_assoc_list |> IntDict.of_list in
     let d1 = dict |> IntDict.remove 1 in
     let l' = d1 |> IntDict.to_list |> List.map (fun (k, _) -> k) in
     l')

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
      ( "add",
        [
          test_case "add two keys then find them" `Quick test_add_find;
          test_case "add with conflict" `Quick test_add_conflict;
        ] );
      ("of_list", [ test_case "small list" `Quick test_of_list ]);
      ( "map",
        [
          test_case "empty" `Quick test_map_empty;
          test_case "small" `Quick test_map_list;
          test_case "int -> string" `Quick test_map_poly;
          test_case "(int, int) -> (string, stirng)" `Quick
            test_map_different_key_types;
        ] );
      ( "fold",
        [
          test_case "sum" `Quick test_fold_sum;
          test_case "fold_right to list" `Quick test_fold_right_list;
        ] );
      ("remove", [ test_case "remove key" `Quick test_remove_key ]);
    ]
