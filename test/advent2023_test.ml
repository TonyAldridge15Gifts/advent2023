open Core
(* Run it *)
let () =
  let open Alcotest in
  let tests = List.join [
    Day1_test.tests;
    Day2_test.tests;
    Day5_test.tests;
  ] in
  run "Advent2023" tests
