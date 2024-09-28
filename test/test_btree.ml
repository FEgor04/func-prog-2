module IntCompare = struct
  type t = int
  let compare = Int.compare
end

module IntDict = Btree.Make(IntCompare)

let passing () = Alcotest.(check int) "same length" 3 ([(1,1); (2, 2); (3,3)]|> IntDict.of_list |> IntDict.length)

let () =
  let open Alcotest in
  run "Btree test" [
    "suite", [ test_case "Test" `Quick passing ]
  ]

