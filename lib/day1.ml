let starts_with = String.starts_with
let ends_with = String.ends_with

open Core

exception No_number of string

let is_digit = function '0' .. '9' -> true | _ -> false

let line_value digit_finder line =
  let left = digit_finder line ~reverse:false in
  let right = digit_finder line ~reverse:true in
  Int.of_string (sprintf "%i%i" left right)

let simple_digit_finder line ~reverse =
  let to_search = match reverse with
  | true -> String.rev line
  | false -> line
  in
  match String.find ~f:is_digit to_search with
  | Some d -> Char.get_digit_exn d
  | _ -> raise (No_number "Oh dear")

let find_stringy_digit word line ~reverse =
  match reverse with
  | false -> starts_with ~prefix:word line
  | true -> ends_with ~suffix:word line

let number_defs = [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]

let rec complex_digit_finder line ~reverse =
  match line with
  | "" -> raise (No_number "Oh dear")
  | _ ->
    let result = List.find number_defs ~f:(fun (word, _) -> find_stringy_digit word line ~reverse) in
    match result with
    | Some (_, digit) -> digit
    | _ -> begin
      let line_length = String.length line in
      let digit = if reverse then (String.get line (line_length - 1)) else (String.get line 0) in
      match Char.get_digit digit with
      | Some d -> d
      | _ -> begin
        let next_string = if reverse then String.slice line 0 (line_length -1) else String.slice line 1 line_length in
        complex_digit_finder next_string ~reverse
      end
    end

let day1 digit_finder =
  let lines = In_channel.read_lines "./data/day1.txt" in
  let numbers = List.map ~f:(line_value digit_finder) lines in
  let summed = List.reduce ~f:(+) numbers in
  match summed with
  | Some n -> n
  | _ -> 0
