open Core

type glyph = {
  glyph: string;
  x_pos: int;
  y_pos: int;
}

type part_number = {
  value: int;
  x_pos: int;
  y_pos: int;
  length: int;
  glyphs: glyph list
}

module Coordinate = struct
  module T = struct
    type t = int * int  [@@deriving compare, sexp_of]
  end
  include T
  include Comparable.Make_plain(T)
end

type collected_gear = part_number list

let number_matcher = Re2.create_exn "[0-9]+"
let glyph_matcher = Re2.create_exn "[^0-9.]"

let find_numbers line y_pos =
  match Re2.get_matches number_matcher line with
  | Ok matches ->
    List.map ~f:(fun matched ->
      let str = Re2.Match.get_exn ~sub:(`Index 0) matched in
      let (pos, length) = Re2.Match.get_pos_exn ~sub:(`Index 0) matched in
      {
        value = Int.of_string str;
        x_pos = pos;
        y_pos;
        length = length;
        glyphs = []
      }
    ) matches
  | _ -> []

let find_glyphs_in_substring substring x_offset y_pos =
  match Re2.get_matches glyph_matcher substring with
  | Ok matches ->
    List.map ~f:(fun matched ->
      let glyph = Re2.Match.get_exn ~sub:(`Index 0) matched in
      let (pos, _) = Re2.Match.get_pos_exn ~sub:(`Index 0) matched in
      {
        glyph;
        x_pos = pos + x_offset;
        y_pos;
      }
    ) matches
  | _ -> []

let get_interesting_slice line x_pos length =
  let actual_start = if x_pos = 0 then 0 else x_pos - 1 in
  let end_pos = x_pos + length + 1 in
  let actual_end = if end_pos > (String.length line) then (String.length line ) else end_pos in
  String.slice line actual_start actual_end


let find_glyphs_for_part ?prev ?next line line_number part_number =
  let grab_glyphs grabbed_line adjusted_line_number =
    let x_pos = part_number.x_pos in
    let actual_start = if x_pos = 0 then 0 else x_pos - 1 in
    let interesting_slice = get_interesting_slice grabbed_line x_pos part_number.length in
    find_glyphs_in_substring interesting_slice actual_start adjusted_line_number
  in

  let prev_glyphs = Option.map prev ~f:(fun p -> grab_glyphs p (line_number - 1)) in
  let next_glyphs = Option.map next ~f:(fun n -> grab_glyphs n (line_number + 1)) in
  let current_glyphs = grab_glyphs line line_number in
  let glyphs = List.concat [prev_glyphs |> Option.value ~default:[]; current_glyphs; next_glyphs |> Option.value ~default:[]] in
  { part_number with glyphs }

let stagger lines =
  let option_lines = List.map lines ~f:Option.return in
  let next_line_lines = List.concat [(List.slice option_lines 1 0); [None]] in
  let prev_line_lines = None :: List.slice option_lines 0 ((List.length lines) - 1) in
  let indexes = List.range 0 (List.length lines) in
  let zipped = List.zip_exn lines next_line_lines |> List.zip_exn prev_line_lines |> List.zip_exn indexes in
  List.map zipped ~f:(fun (idx, (prev, (current, next))) -> idx, prev, current, next)

let parse_line ?prev ?next line line_number =
  let numbers = find_numbers line line_number in
  List.map ~f:(find_glyphs_for_part ?prev ?next line line_number) numbers

let parse lines =
  let staggered = stagger lines in
  List.map staggered ~f:(fun (idx, prev, current, next) ->
    parse_line ?prev ?next current idx
  ) |> List.concat

let day3 () =
  let lines = In_channel.read_lines "./data/day3.txt" in
  let parsed = parse lines in
  let valid_parts = List.filter parsed ~f:(fun part -> (List.length part.glyphs) > 0) in
  List.reduce_exn ~f:(+) (List.map valid_parts ~f:(fun part -> part.value))

let find_gear_ratios part_numbers =
  let possible_gears = List.filter part_numbers ~f:(fun part -> List.exists part.glyphs ~f:(fun g -> String.equal g.glyph "*")) in
  let empty_acc = Map.empty (module Coordinate) in
  let collected = List.fold possible_gears ~init:empty_acc ~f:(fun acc part -> 
    let cog_glyphs = List.filter part.glyphs ~f:(fun g -> String.equal g.glyph "*") in
    List.fold cog_glyphs ~init:acc ~f:(fun acc glyph -> 
      let x = glyph.x_pos in
      let y = glyph.y_pos in
      Map.add_multi acc ~key:(x, y) ~data:part
    )
  ) in
  let only_pairs = Map.filter collected ~f:(fun values -> (List.length values = 2)) in
  Map.data only_pairs |> List.map ~f:(fun parts -> (List.map ~f:(fun a -> a.value) parts) |> List.reduce_exn ~f:( * )) |> List.reduce_exn ~f:( + )


let day3_2 () =
  let lines = In_channel.read_lines "./data/day3.txt" in
  let parsed = parse lines in
  find_gear_ratios parsed
