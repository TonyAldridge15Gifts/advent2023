open Core

let number_matcher = Re2.create_exn "[0-9]+"

let pair_matcher = Re2.create_exn "([0-9]+) ([0-9]+)"

exception Bad_seeds of string

let parse_seeds line =
  Parser.find_all_matches number_matcher line |> List.map ~f:Int.of_string

module Range = struct
  type t = {
    start_pos: int;
    end_pos: int;
  }
  [@@deriving compare, show]

  type intersection_and_remainder_result = {
    matched: t option;
    before: t option;
    after: t option;
  }
  [@@deriving compare, show]

  let equal a b = (compare a b) = 0
  let equal_intersection_and_remainder_result a b= (compare_intersection_and_remainder_result a b) = 0

  let make start_pos end_pos =
    {start_pos ; end_pos }

  let make_intersection before matched after =
    { before ; matched ; after }

  let from_start_and_count start count =
    make start (start + count - 1)

  let contains t value =
    (value >= t.start_pos) && value < t.end_pos

  let intersection_and_remainder t matcher : intersection_and_remainder_result =
    let before = if (t.start_pos < matcher.start_pos) then
      Some (make t.start_pos (min (matcher.start_pos - 1) t.end_pos))
    else None in
    let matched = if (t.end_pos < matcher.start_pos || t.start_pos > matcher.end_pos) then
      None
    else
      Some (make (max t.start_pos matcher.start_pos) (min t.end_pos matcher.end_pos))
    in
    let after = if (t.end_pos > matcher.end_pos) then
      Some (make (min (t.end_pos + 1) (matcher.end_pos + 1)) t.end_pos)
    else None in
    { before ; matched ; after }
end

module Mapping = struct
  module Mapping_line = struct
    type t = {
      source: Range.t;
      destination: Range.t;
    }

    exception Bad_line of string

    let parse_line line : t = 
      match Parser.find_all_matches number_matcher line |> List.map ~f:Int.of_string with
      | [destination_start; source_start; range] -> {
        source=(Range.make source_start range);
        destination=(Range.make destination_start range);
      }
      | _ -> raise (Bad_line line)

    let maybe_map_input (t : t) (input : int) : int option =
      if Range.contains t.source input then 
        Some (input - t.source.start_pos + t.destination.start_pos)
      else
        None

    (*let map_range (t : t) (input : Range.t) : Range.t list =
      let { matched; before; after } : Range.intersection_and_remainder_result = Range.intersection_and_remainder input t.source in
      (* rebase Matched to the destination range *)
      (* return before and after *)
      []*)
  end

  type t = {
    from_group: string;
    to_group: string;
    lines: Mapping_line.t list;
  }

  let title_matcher = Re2.create_exn "^([a-z]+)-to-([a-z]+) map:$"
  exception Bad_title of string

  let parse_title line =
    match Parser.find_all_submatches title_matcher line with
    | [_ ; (Some to_group) ; (Some from_group)] :: _ ->
      from_group, to_group
    | _ -> raise (Bad_title line)

  let parse_mapping lines : (t * string list) =
    let (from_group, to_group) = parse_title (List.hd_exn lines) in
    let rec parse_mapping_line output = function
      | "" :: rest -> output, rest
      | [] -> output, []
      | line :: rest -> parse_mapping_line ((Mapping_line.parse_line line) :: output) rest
    in
    let parsed_lines, remainder = parse_mapping_line [] (List.tl_exn lines) in
    {
      from_group;
      to_group;
      lines = parsed_lines
    }, remainder

    let map_input (t : t) (input : int) : int =
      let mapped = List.find_map t.lines ~f:(fun row -> Mapping_line.maybe_map_input row input) in
      match mapped with
      | Some v -> v
      | None -> input

end

let parse_seed_ranges line =
  let matches = Parser.find_all_submatches pair_matcher line in
  List.map matches ~f:(fun matched ->
    matched |> List.tl_exn |> List.filter_opt |> (function
      | [start; range] -> Range.from_start_and_count (Int.of_string start) (Int.of_string range)
      | _ -> raise (Bad_seeds line)
    )
  ) 

let parse lines =
  let seeds = parse_seeds (List.hd_exn lines) in
  let rec parse_mappings output = function
    | [] -> List.rev output
    | line -> 
        let parsed_mapping, remaining_lines = Mapping.parse_mapping line in
        parse_mappings (parsed_mapping :: output) remaining_lines
  in
  seeds, parse_mappings [] (List.slice lines 2 0)

let parse_with_ranges lines =
  let seeds = parse_seed_ranges (List.hd_exn lines) in
  let rec parse_mappings output = function
    | [] -> List.rev output
    | line -> 
        let parsed_mapping, remaining_lines = Mapping.parse_mapping line in
        parse_mappings (parsed_mapping :: output) remaining_lines
  in
  seeds, parse_mappings [] (List.slice lines 2 0)

let map_input_chain mappings input =
  List.fold ~init:input ~f:(fun acc mapping -> Mapping.map_input mapping acc) mappings

let day5 () =
  let lines = In_channel.read_lines "./data/day5.txt" in
  let inputs, mappings = parse lines in
  let outputs = List.map inputs ~f:(map_input_chain mappings) in
  List.min_elt outputs ~compare:(-) |> Option.value ~default:0

let day5_2 () =
  let lines = In_channel.read_lines "./data/day5.txt" in
  let inputs, mappings = parse_with_ranges lines in
  inputs, mappings
