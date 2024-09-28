let passing =
  QCheck.Test.make ~count:1000
    ~name:"list_rev_is_involutive"
    QCheck.(list small_int)
    (fun l -> List.rev (List.rev l) = l);;

let failing =
  QCheck.Test.make ~count:10
    ~name:"fail_sort_id"
    QCheck.(list small_int)
    (fun l -> l = List.sort compare l);;


let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [ passing; failing]
  in
  Alcotest.run "my test" [
    "suite", suite
  ]

