open Base

module Parse_color = struct
  let test_parse_color str expected () =
    Alcotest.(check bool) "is equal" true ((Advent2023.Day2.parse_color str) |> phys_equal expected)

  let tests = [
    Alcotest.test_case "Parse red" `Quick (test_parse_color "red" Advent2023.Day2.Red);
    Alcotest.test_case "Parse green" `Quick (test_parse_color "green" Advent2023.Day2.Green);
    Alcotest.test_case "Parse blue" `Quick (test_parse_color "blue" Advent2023.Day2.Blue)
  ]
end

let tests = [
  "parse_color", Parse_color.tests;
  (*"simple_digit_finder", Simple_digit_finder.tests;
  "find_stringy_digit", Find_stringy_digit.tests;
  "complex_digit_finder", Complex_digit_finder.tests;*)
]

