open! Format

let list_split_idx idx lst =
  let is_left i _ = i < idx in
  let is_right i _ = i >= idx in
  let list_left = List.filteri is_left lst in
  let list_right = List.filteri is_right lst in
  (list_left, list_right)

let%expect_test "split, n is odd" =
  let l = [ 1; 2; 3; 4; 5 ] in
  printf "A list [l]\n";
  let[@warning "-partial-match"] left, mid :: right = list_split_idx 2 l in
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

let%expect_test "split, n is even" =
  let l = [ 1; 2; 3; 4 ] in
  printf "A list [l]\n";
  let[@warning "-partial-match"] left, mid :: right = list_split_idx 2 l in
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
    Right: 4
    |}]

(** Appends all elements from l2 to l1 before index `idx` *)
let append_at l1 l2 idx =
  let l1_left, l1_right = list_split_idx idx l1 in
  l1_left @ l2 @ l1_right

let%expect_test "append_at middle" =
  let l1 = [ 1; 5; 6 ] in
  let l2 = [ 2; 3; 4 ] in
  let l = append_at l1 l2 1 in
  let print_with_space x = printf " %d" x in
  List.iter print_with_space l1;
  printf "\n";
  List.iter print_with_space l2;
  printf "\n";
  List.iter print_with_space l;
  printf "\n";
  [%expect {|
    1 5 6
    2 3 4
    1 2 3 4 5 6
    |}]

let%expect_test "append_at beggining" =
  let l1 = [ 4; 5; 6 ] in
  let l2 = [ 1; 2; 3 ] in
  let l = append_at l1 l2 0 in
  let print_with_space x = printf " %d" x in
  List.iter print_with_space l1;
  printf "\n";
  List.iter print_with_space l2;
  printf "\n";
  List.iter print_with_space l;
  printf "\n";
  [%expect {|
    4 5 6
    1 2 3
    1 2 3 4 5 6
    |}]

let%expect_test "append_at end" =
  let l1 = [ 1; 2; 3 ] in
  let l2 = [ 4; 5; 6 ] in
  let l = append_at l1 l2 3 in
  let print_with_space x = printf " %d" x in
  List.iter print_with_space l1;
  printf "\n";
  List.iter print_with_space l2;
  printf "\n";
  List.iter print_with_space l;
  printf "\n";
  [%expect {|
    1 2 3
    4 5 6
    1 2 3 4 5 6
    |}]

(** Returns index of a first element that is greater or equal to current *)
let lower_bound lst value comparator =
  let is_good x = comparator x value >= 0 in
  let idx = List.find_index is_good lst in
  match idx with
  (* All elements are lower than `value` *)
  | None -> List.length lst
  | Some idx -> idx

let add_to_sorted lst value comparator =
  let idx = lower_bound lst value comparator in
  append_at lst [ value ] idx

let%expect_test "add_to_sorted middle" =
  let value = 3 in
  let l1 = [ 1; 2; 4 ] in
  let l = add_to_sorted l1 value Int.compare in
  let print_with_space x = printf " %d" x in
  List.iter print_with_space l1;
  printf "\n";
  List.iter print_with_space l;
  printf "\n";
  [%expect {|
    1 2 4
    1 2 3 4
    |}]

let%expect_test "add_to_sorted start" =
  let value = 1 in
  let l1 = [ 2; 3; 4 ] in
  let l = add_to_sorted l1 value Int.compare in
  let print_with_space x = printf " %d" x in
  List.iter print_with_space l1;
  printf "\n";
  List.iter print_with_space l;
  printf "\n";
  [%expect {|
    2 3 4
    1 2 3 4
    |}]

let%expect_test "add_to_sorted end" =
  let value = 10 in
  let l1 = [ 2; 3; 4 ] in
  let l = add_to_sorted l1 value Int.compare in
  let print_with_space x = printf " %d" x in
  List.iter print_with_space l1;
  printf "\n";
  List.iter print_with_space l;
  printf "\n";
  [%expect {|
    2 3 4
    2 3 4 10
    |}]

let%expect_test "add_to_sorted equal" =
  let value = 3 in
  let l1 = [ 1; 2; 3; 4; 5 ] in
  let l = add_to_sorted l1 value Int.compare in
  let print_with_space x = printf " %d" x in
  List.iter print_with_space l1;
  printf "\n";
  List.iter print_with_space l;
  printf "\n";
  [%expect {|
    1 2 3 4 5
    1 2 3 3 4 5
    |}]

(** Updates the element at index `idx` in the list `lst` with the value `new_value`.
      If `idx` is out of bounds, it raises `Invalid_argument`. *)
let update_at lst idx new_value =
  if idx < 0 || idx >= List.length lst then
    invalid_arg "Utils.update_at: index out of bounds"
  else List.mapi (fun i x -> if i = idx then new_value else x) lst

let rec list_last = function
  | [] -> None
  | [ x ] -> Some x
  | _x :: hd -> list_last hd
