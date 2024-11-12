module IntCompare = struct
  type t = int

  let compare = Int.compare
end

module BTreeConfig = struct
  let t = 10
end

module IntDict = Btree.Make (IntCompare) (BTreeConfig)

let add_from_list_has_key =
  QCheck.Test.make ~count:100 ~name:"add_from_list_has_key"
    QCheck.(list int)
    (fun lst_raw ->
      let lst = lst_raw |> List.sort_uniq Int.compare in
      let to_assoc x = (x, x) in
      let lst_assoc = lst |> List.map to_assoc in
      let dict = lst_assoc |> IntDict.of_list in
      let has_key item = dict |> IntDict.has item in
      let has_all = lst |> List.map has_key |> List.fold_left ( && ) true in
      has_all)

let () =
  let add_suite =
    List.map QCheck_alcotest.to_alcotest [ add_from_list_has_key ]
  in
  Alcotest.run "quickcheck" [ ("add_suite", add_suite) ]
