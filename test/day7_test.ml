module Process_test = struct
  open Advent2023.Day7

  let test_example () =
    let result = process [
      "32T3K 765" ;
      "T55J5 684" ;
      "KK677 28" ;
      "KTJJT 220" ;
      "QQQJA 483" ;
    ] in
    Alcotest.(check int) "Not equal" 6440 result

  let test_advanced () =
    let result = process ~jokers:true [
      "32T3K 765" ;
      "T55J5 684" ;
      "KK677 28" ;
      "KTJJT 220" ;
      "QQQJA 483" ;
    ] in
    Alcotest.(check int) "Not equal" 5905 result

  let tests = [
    Alcotest.test_case "Basic example" `Quick test_example;
    Alcotest.test_case "Advanced example" `Quick test_advanced;
  ]
end

let tests = [
  "process", Process_test.tests;
]


