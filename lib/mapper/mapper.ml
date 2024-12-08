module Make
    (Key1 : Btree.OrderedType)
    (Key2 : Btree.OrderedType)
    (Config : Btree.BTreeConfig) =
struct
  module Dict1 = Btree.Make (Key1) (Config)
  module Dict2 = Btree.Make (Key2) (Config)

  let map f tree = tree |> Dict1.to_list |> List.map f |> Dict2.of_list
end
