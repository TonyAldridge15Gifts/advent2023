open Core

module Winning_numbers = struct
  type t = (int, Int.comparator_witness) Set.t

  let parse (numbers : string) : t =
    String.split ~on:' ' numbers |> List.filter ~f:(String.(<>) "") |> List.map ~f:(Int.of_string) |> Set.of_list (module Int)
end

module Picked = struct
  type t = (int, Int.comparator_witness) Set.t

  let parse (numbers : string) : t =
    String.split ~on:' ' numbers |> List.filter ~f:(String.(<>) "") |> List.map ~f:(Int.of_string) |> Set.of_list (module Int)

  let get_matched_number_count my_numbers winning_numbers : int =
    Set.inter my_numbers winning_numbers |> Set.count ~f:(fun _ -> true)

  let get_score (match_count: int) : int =
    if match_count = 0 then 0 else (Int.pow 2 (match_count - 1))
end

module Scratchcard = struct
  type t = int * Winning_numbers.t * Picked.t

  let line_matcher = Re2.create_exn "^Card[\\s]+([0-9]+):([0-9 ]+)\\|([0-9 ]+)$"

  let parse line : t option =
    match Re2.get_matches line_matcher line with
    | Ok (matched :: _) ->
      begin
        match Re2.Match.get_all (Re2.without_trailing_none matched) with
        | [| _ ; (Some card_number) ; (Some winning_numbers) ; (Some picked_numbers) |] ->
            Some ((Int.of_string card_number), (Winning_numbers.parse winning_numbers), (Picked.parse picked_numbers))
        | _ -> None
      end
    | _ -> None

  let calculate_score (card : t) : int =
    let (_, winning_numbers, scratchcard) = card in
    let matched_count = Picked.get_matched_number_count scratchcard winning_numbers in
    Picked.get_score matched_count

  let find_subsequent_cards (all_cards: t list) (my_card : t) =
    let (card_number, winning_numbers, scratchcard) = my_card in
    let matched_count = Picked.get_matched_number_count scratchcard winning_numbers in
    match matched_count with
    | 0 -> []
    | _ -> 
      List.slice all_cards card_number (card_number + matched_count)

end

let day4 () =
  let lines = In_channel.read_lines "./data/day4.txt" in
  let parsed = List.filter_map ~f:Scratchcard.parse lines in
  let scores = List.map ~f:Scratchcard.calculate_score parsed in
  List.reduce_exn ~f:(+) scores

let day4_2 () =
  let lines = In_channel.read_lines "./data/day4.txt" in
  let parsed = List.filter_map ~f:Scratchcard.parse lines in
  let memo_find = Memo.general (Scratchcard.find_subsequent_cards parsed) in
  let rec recursive_count active_cards current_score =
    let next_lines = List.map active_cards ~f:memo_find in
    let flattened = List.concat next_lines in
    match flattened with
    | [] -> current_score
    | _ -> recursive_count flattened (current_score + List.length flattened) in
  recursive_count parsed (List.length parsed)
