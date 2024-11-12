module IntCompare = struct
  type t = int

  let compare = Int.compare
end

module BTreeConfig = struct
  let t = 10
end

module IntDict = Btree.Make (IntCompare) (BTreeConfig)

let to_assoc x = (x, x)
let to_assoc_lst = List.map to_assoc

let add_from_list_has_key =
  QCheck.Test.make ~count:50 ~name:"add_from_list_has_key"
    QCheck.(list int)
    (fun lst_raw ->
      let lst = lst_raw |> List.sort_uniq Int.compare in
      let lst_assoc = lst |> to_assoc_lst in
      let dict = lst_assoc |> IntDict.of_list in
      let has_key item = dict |> IntDict.has item in
      let has_all = lst |> List.map has_key |> List.fold_left ( && ) true in
      has_all)

let of_list_to_list_is_identity =
  QCheck.Test.make ~count:50 ~name:"of_list_to_list_is_identity"
    QCheck.(list int)
    (fun lst_raw ->
      let lst = lst_raw |> List.sort_uniq Int.compare in
      let lst_assoc = lst |> to_assoc_lst in
      let dict = lst_assoc |> IntDict.of_list in
      let lst1 = dict |> IntDict.to_list in
      lst_assoc = lst1)

let () =
  let add_suite =
    List.map QCheck_alcotest.to_alcotest [ add_from_list_has_key ]
  in
  let of_list_to_list =
    List.map QCheck_alcotest.to_alcotest [ of_list_to_list_is_identity ]
  in
  Alcotest.run "quickcheck"
    [ ("add_suite", add_suite); ("of_list_to_list", of_list_to_list) ]
