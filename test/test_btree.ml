module IntCompare = struct
  type t = int

  let compare = Int.compare
end

module BTreeConfig = struct
  let t = 10
end

module IntDict = Btree.Make (IntCompare) (BTreeConfig)
