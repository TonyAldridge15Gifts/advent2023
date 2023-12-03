open Core

type color =
  | Red
  | Blue
  | Green

type color_and_count = color * int
type pull = color_and_count list
type game = { game : int; pulls : pull list }

type color_and_count_constraint = {
  red: int;
  green: int;
  blue: int;
}

exception Bad_color of string
exception Bad_color_and_count of string
exception Bad_game of string
exception Bad_line of string

let parse_color color_string =
  match color_string with
  | "red" -> Red
  | "blue" -> Blue
  | "green" -> Green
  | _ -> raise (Bad_color color_string)

let parse_color_and_count color_count_string =
  let (count_string, color_string) = match String.split ~on:' ' color_count_string with
  | count_string :: color_string :: _ ->  (count_string, color_string)
  | _ -> raise (Bad_color_and_count color_count_string) in
  let count = Int.of_string count_string in
  let color = parse_color color_string in
  color, count

let parse_color_and_count color_and_count_string =
  let colors = String.split ~on:',' color_and_count_string |> List.map ~f:String.strip in
  List.map ~f:parse_color_and_count colors

let parse_game_number game_string =
  match String.split ~on:' ' game_string with
  | _ :: game_number :: _ ->  Int.of_string game_number
  | _ -> raise (Bad_game game_string)

let parse_line line =
  match String.split ~on:':' line with
  | game_string :: pull_list :: _ ->
      let color_and_count_strings = String.split ~on:';' pull_list |> List.map ~f:String.strip in
      { game = (parse_game_number game_string) ; pulls = (List.map ~f:parse_color_and_count color_and_count_strings) }
  | _ -> raise (Bad_line line)

let parse_all lines =
  List.map ~f:parse_line lines

let is_valid_color_and_count color_and_count_constraint color_and_count =
  match color_and_count with
  | Red, r -> r <= color_and_count_constraint.red
  | Blue, b -> b <= color_and_count_constraint.blue
  | Green, g -> g <= color_and_count_constraint.green
  
let is_valid_pull color_and_count_constraint pull =
  List.for_all ~f:(is_valid_color_and_count color_and_count_constraint) pull

let is_valid_game color_and_count_constraint game =
  List.for_all ~f:(is_valid_pull color_and_count_constraint) game.pulls

let filter_to_valid color_and_count_constraint game_list =
  List.filter ~f:(is_valid_game color_and_count_constraint) game_list

let day2 color_and_count_constraint =
  let lines = In_channel.read_lines "./data/day2.txt" in
  let games = parse_all lines in
  let filtered = filter_to_valid color_and_count_constraint games in
  match List.reduce ~f:(+) (List.map ~f:(fun game -> game.game) filtered) with
  | Some x -> x
  | None -> 0

let rec minimum_for_pull current pull =
  match pull with
  | color_and_count :: rest ->
    begin
      match color_and_count with
      | Red, r -> minimum_for_pull {
        current with red = current.red + r
      } rest
      | Blue, b -> minimum_for_pull {
        current with blue = current.blue + b
      } rest
      | Green, g -> minimum_for_pull {
        current with green = current.green + g
      } rest
    end
  | [] -> current

let minimum_of_two a b =
  {
    red = max a.red b.red;
    green = max a.green b.green;
    blue = max a.blue b.blue;
  }

let minimum_for_game game =
  List.reduce_exn ~f:minimum_of_two (List.map ~f:(minimum_for_pull { red = 0; green = 0; blue = 0 }) game.pulls)

let game_power game =
  let minimums = minimum_for_game game in
  minimums.red * minimums.green * minimums.blue

let day2_2 () =
  let lines = In_channel.read_lines "./data/day2.txt" in
  let games = parse_all lines in
  List.reduce_exn ~f:(+) (List.map ~f:game_power games)
