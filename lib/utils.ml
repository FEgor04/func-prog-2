open! Format

let list_split_idx idx lst =
  let is_left i _ = i < idx in
  let is_right i _ = i >= idx in
  let list_left = List.filteri is_left lst in
  let[@warning "-partial-match"] (middle :: list_right) =
    List.filteri is_right lst
  in
  (list_left, middle, list_right)

let%expect_test "interleaved" =
  let l = [ 1; 2; 3; 4; 5 ] in
  printf "A list [l]\n";
  let left, mid, right = list_split_idx 2 l in
  let print_with_space x = printf " %d" x in
  printf "Left:";
  List.iter print_with_space left;
  printf "\nMid:";
  print_with_space mid;
  printf "\nRight:";
  List.iter print_with_space right;
  [%expect {|
    A list [l]
    Left: 1 2
    Mid: 3
    Right: 4 5
    |}]
