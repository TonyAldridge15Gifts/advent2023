open Core

type part_number = {
  value: int;
  start_pos: int;
  length: int;
  glyphs: string list
}

let number_matcher = Re2.create_exn "[0-9]+"
let glyph_matcher = Re2.create_exn "[^0-9.]"

let find_numbers line =
  match Re2.get_matches number_matcher line with
  | Ok matches ->
    List.map ~f:(fun matched ->
      let str = Re2.Match.get_exn ~sub:(`Index 0) matched in
      let (pos, length) = Re2.Match.get_pos_exn ~sub:(`Index 0) matched in
      {
        value = Int.of_string str;
        start_pos = pos;
        length = length;
        glyphs = []
      }
    ) matches
  | _ -> []

let find_glyphs_in_substring substring =
  Re2.find_all glyph_matcher substring

let get_interesting_slice line start_pos length =
  let actual_start = if start_pos = 0 then 0 else start_pos - 1 in
  let end_pos = start_pos + length + 1 in
  let actual_end = if end_pos > (String.length line) then (String.length line ) else end_pos in
  String.slice line actual_start actual_end


let find_glyphs_for_part ?prev ?next line part_number =
  let prev_glyphs = match prev with
  | Some prev_line -> find_glyphs_in_substring (get_interesting_slice prev_line part_number.start_pos part_number.length) |> Or_error.ok |> Option.value ~default:[]
  | None -> [] in
  let next_glyphs = match next with
  | Some next_line -> find_glyphs_in_substring (get_interesting_slice next_line part_number.start_pos part_number.length) |> Or_error.ok |> Option.value ~default:[]
  | None -> [] in
  let current_glyphs = find_glyphs_in_substring (get_interesting_slice line part_number.start_pos part_number.length) |> Or_error.ok |> Option.value ~default:[] in
  let glyphs = List.concat [prev_glyphs; current_glyphs; next_glyphs] in
  { part_number with glyphs }

let stagger lines =
  let option_lines = List.map lines ~f:Option.return in
  let next_line_lines = List.concat [(List.slice option_lines 1 0); [None]] in
  let prev_line_lines = None :: List.slice option_lines 0 ((List.length lines) - 1) in
  let zipped = List.zip_exn lines next_line_lines |> List.zip_exn prev_line_lines in
  List.map zipped ~f:(fun (prev, (current, next)) -> prev, current, next)

let parse_line ?prev ?next line =
  let numbers = find_numbers line in
  List.map ~f:(find_glyphs_for_part ?prev ?next line) numbers

let parse lines =
  let staggered = stagger lines in
  List.map staggered ~f:(fun (prev, current, next) ->
    parse_line ?prev ?next current
  ) |> List.concat

let day3 () =
  let lines = In_channel.read_lines "./data/day3.txt" in
  let parsed = parse lines in
  let valid_parts = List.filter parsed ~f:(fun part -> (List.length part.glyphs) > 0) in
  List.reduce_exn ~f:(+) (List.map valid_parts ~f:(fun part -> part.value))
