open Core

let number_matcher = Re2.create_exn "[0-9]+"
exception Parse_error of string list

let factor_pairs number =
  let floating_number = Float.of_int number in
  let root = sqrt (Float.of_int number) |> Float.round_down |> Int.of_float in
  let range = List.range 1 (root + 1) in
  List.map range ~f:(fun idx ->
    let divided = floating_number /. (Float.of_int idx) in
    match Float.mod_float divided 1.0 with
    | 0. -> Some (idx, (Int.of_float divided))
    | _ -> None
  ) |> List.filter_opt


module Race = struct
  type t = {
    time: int ;
    distance: int
  }

  let parse_numbers line =
    Parser.find_all_matches number_matcher line |> List.map ~f:Int.of_string

  let parse_all lines : t list =
    match lines with
    | time_line :: distance_line :: _ ->
      let times = parse_numbers time_line in
      let best_distances = parse_numbers distance_line in
      List.zip_exn times best_distances |> List.map ~f:(fun (time, distance) ->
        { time ; distance }
      )
    | _ -> raise (Parse_error lines)

end
