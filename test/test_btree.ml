module IntCompare = struct
  type t = int

  let compare = Int.compare
end

module BTreeConfig = struct
  let t = 10
end

module IntDict = Btree.Make (IntCompare) (BTreeConfig)

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
    ]
