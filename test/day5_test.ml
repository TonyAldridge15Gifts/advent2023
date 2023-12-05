module Range_test = struct
  open Advent2023.Day5

  let range_check =
    Alcotest.testable Range.pp Range.equal

  let intersection_check =
    Alcotest.testable Range.pp_intersection_and_remainder_result Range.equal_intersection_and_remainder_result

  let test_exact_match () =
    let value = Range.make 5 10 in
    let matcher = Range.make 5 10 in
    let expected = Range.make_intersection None (Some value) None in
    let result = Range.intersection_and_remainder value matcher in
    Alcotest.(check intersection_check) "Not equal" expected result
    
  let test_inner_match () =
    let value = Range.make 6 9 in
    let matcher = Range.make 5 10 in
    let expected = Range.make_intersection None (Some value) None in
    let result = Range.intersection_and_remainder value matcher in
    Alcotest.(check intersection_check) "Not equal" expected result

  let test_all_before_range () =
    let value = Range.make 1 4 in
    let matcher = Range.make 5 10 in
    let expected = Range.make_intersection (Some value) None None in
    let result = Range.intersection_and_remainder value matcher in
    Alcotest.(check intersection_check) "Not equal" expected result

  let test_all_after_range () =
    let value = Range.make 11 15 in
    let matcher = Range.make 5 10 in
    let expected = Range.make_intersection None None (Some value) in
    let result = Range.intersection_and_remainder value matcher in
    Alcotest.(check intersection_check) "Not equal" expected result

  let test_overlap_start () =
    let value = Range.make 1 7 in
    let matcher = Range.make 5 10 in
    let expected = Range.make_intersection (Some (Range.make 1 4)) (Some (Range.make 5 7)) None in
    let result = Range.intersection_and_remainder value matcher in
    Alcotest.(check intersection_check) "Not equal" expected result

  let test_overlap_end () =
    let value = Range.make 9 13 in
    let matcher = Range.make 5 10 in
    let expected = Range.make_intersection None (Some (Range.make 9 10)) (Some (Range.make 11 13)) in
    let result = Range.intersection_and_remainder value matcher in
    Alcotest.(check intersection_check) "Not equal" expected result

  let test_double_overlap () =
    let value = Range.make 1 15 in
    let matcher = Range.make 5 10 in
    let expected = Range.make_intersection (Some (Range.make 1 4)) (Some (Range.make 5 10)) (Some (Range.make 11 15)) in
    let result = Range.intersection_and_remainder value matcher in
    Alcotest.(check intersection_check) "Not equal" expected result

  let tests = [
    Alcotest.test_case "Exact match" `Quick test_exact_match;
    Alcotest.test_case "Inner match" `Quick test_inner_match;
    Alcotest.test_case "All before range" `Quick test_all_before_range;
    Alcotest.test_case "All after range" `Quick test_all_after_range;
    Alcotest.test_case "Overlap start" `Quick test_overlap_start;
    Alcotest.test_case "Overlap end" `Quick test_overlap_end;
    Alcotest.test_case "Double overlap" `Quick test_double_overlap;
  ]
end

let tests = [
  "range", Range_test.tests;
]

