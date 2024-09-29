module IntCompare = struct
  type t = int

  let compare = Int.compare
end

module IntDict = Btree.Make (IntCompare)
module IntSet = Set.Make (IntCompare)

let to_assoc l = l |> List.map (fun x -> (x, x))
let to_unique l = l |> IntSet.of_list |> IntSet.to_list

let of_list_to_list =
  QCheck.Test.make ~count:1000 ~name:"of_list -> to_list returns the same"
    QCheck.(list int)
    (fun l ->
      l |> to_unique |> to_assoc |> IntDict.of_list |> IntDict.to_list
      = (l |> to_unique |> to_assoc))

let () =
  let suite = List.map QCheck_alcotest.to_alcotest [ of_list_to_list ] in
  Alcotest.run "my test" [ ("suite", suite) ]
