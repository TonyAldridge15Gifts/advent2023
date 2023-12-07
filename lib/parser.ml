open Core

let find_all_matches regex line =
  Re2.find_all_exn regex line

let find_all_submatches regex line : string option list list =
  let matches = Re2.get_matches_exn regex line in
  List.map matches ~f:(fun matched ->
    Re2.Match.get_all (Re2.without_trailing_none matched) |> List.of_array
  )

let find_first_submatches regex line : string option list =
  find_all_submatches regex line |> List.hd_exn
