module IntCompare = struct
  type t = int

  let compare = Int.compare
end

module BTreeConfig = struct
  let t = 10
end

module IntDict = Btree.Make (IntCompare) (BTreeConfig)

let _ =
  QCheck.Test.make ~count:1000 ~name:"add_from_list_has_key"
    QCheck.(list int)
    (fun lst_raw ->
      let lst = lst_raw |> List.sort_uniq Int.compare in
      let add_list_item acc item = IntDict.add item item acc in
      let dict = lst |> List.fold_left add_list_item IntDict.empty in
      let has_key item = dict |> IntDict.find item |> Option.is_some in
      lst |> List.map has_key |> List.fold_left ( && ) true)

let () =
  let add_suite = List.map QCheck_alcotest.to_alcotest [] in
  Alcotest.run "quickcheck" [ ("add_suite", add_suite) ]
