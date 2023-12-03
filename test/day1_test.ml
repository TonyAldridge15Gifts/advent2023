module Is_digit = struct
  let test_is_digit () =
    Alcotest.(check bool) "is true" true (Advent2023.Day1.is_digit '1')

  let test_is_not_digit () =
    Alcotest.(check bool) "is false" false (Advent2023.Day1.is_digit 'a')

  let tests = [
    Alcotest.test_case "Is a digit" `Quick test_is_digit;
    Alcotest.test_case "Is not a digit" `Quick test_is_not_digit;
  ]
end

module Simple_digit_finder = struct
  let test_find_digit () =
    Alcotest.(check int) "Is 5" 5 (Advent2023.Day1.simple_digit_finder "aa54bb" ~reverse:false)

  let test_find_digit_reverse () =
    Alcotest.(check int) "Is 4" 4 (Advent2023.Day1.simple_digit_finder "aa54bb" ~reverse:true)

  let tests = [
    Alcotest.test_case "Find digit" `Quick test_find_digit;
    Alcotest.test_case "Find a digit in reverse" `Quick test_find_digit_reverse;
  ]
end

module Find_stringy_digit = struct
  let test_find_digit () =
    Alcotest.(check bool) "Is true" true (Advent2023.Day1.find_stringy_digit "one" "onellama" ~reverse:false)

  let test_find_digit_fail () =
    Alcotest.(check bool) "Is false" false (Advent2023.Day1.find_stringy_digit "one" "llamaone" ~reverse:false)

  let test_find_digit_reverse () =
    Alcotest.(check bool) "Is true" true (Advent2023.Day1.find_stringy_digit "llama" "onellama" ~reverse:true)

  let test_find_digit_reverse_fail () =
    Alcotest.(check bool) "Is false" false (Advent2023.Day1.find_stringy_digit "llama" "llamaone" ~reverse:true)

  let tests = [
    Alcotest.test_case "Find digit" `Quick test_find_digit;
    Alcotest.test_case "Find digit failure" `Quick test_find_digit_fail;
    Alcotest.test_case "Find a digit in reverse" `Quick test_find_digit_reverse;
    Alcotest.test_case "Find a digit in reverse failure" `Quick test_find_digit_reverse_fail;
  ]
end

module Complex_digit_finder = struct
  let test_find_digit () =
    Alcotest.(check int) "Is 5" 5 (Advent2023.Day1.complex_digit_finder "aa54bb" ~reverse:false)

  let test_find_digit_reverse () =
    Alcotest.(check int) "Is 4" 4 (Advent2023.Day1.complex_digit_finder "aa54bb" ~reverse:true)

  let test_find_word() =
    Alcotest.(check int) "Is 4" 4 (Advent2023.Day1.complex_digit_finder "aafour54fivebb" ~reverse:false)

  let test_find_word_reverse () =
    Alcotest.(check int) "Is 5" 5 (Advent2023.Day1.complex_digit_finder "aafour54fivebb" ~reverse:true)

  let tests = [
    Alcotest.test_case "Find digit" `Quick test_find_digit;
    Alcotest.test_case "Find a digit in reverse" `Quick test_find_digit_reverse;
    Alcotest.test_case "Find word" `Quick test_find_word;
    Alcotest.test_case "Find a word in reverse" `Quick test_find_word_reverse;
  ]
end

let tests = [
  "is_digit", Is_digit.tests;
  "simple_digit_finder", Simple_digit_finder.tests;
  "find_stringy_digit", Find_stringy_digit.tests;
  "complex_digit_finder", Complex_digit_finder.tests;
]
