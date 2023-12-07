open Core

module Card = struct
  type t = int
  exception Compare_error

  let of_char = function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | v -> Char.get_digit_exn v

  let compare a b =
    b - a

  let rec compare_list a b =
    match a, b with
    | ([]), ([]) -> 0
    | (a :: rest_a), (b :: rest_b) -> begin
      match b - a with
        | 0 -> compare_list rest_a rest_b
        | otherwise -> otherwise
      end
    | _ -> raise Compare_error
end

module HandValue = struct
  type t =
    | HighCard
    | Pair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind
    [@@deriving enum]

  let calculate_from_cards cards =
    let collected = List.sort_and_group ~compare:(-) cards in
    let has_group_of_size l size = List.exists l ~f:(fun g -> (List.length g) = size) in
    let count_group_of_size l size = (List.filter l ~f:(fun g -> (List.length g) = size)) |> List.length in
    if has_group_of_size collected 5 then
      FiveOfAKind
    else if has_group_of_size collected 4 then
      FourOfAKind
    else if (has_group_of_size collected 3) && (has_group_of_size collected 2) then
      FullHouse
    else if has_group_of_size collected 3 then
      ThreeOfAKind
    else if (count_group_of_size collected 2) = 2 then
      TwoPair
    else if has_group_of_size collected 2 then
      Pair
    else
      HighCard
end

module Hand = struct
  type t = (Card.t list) * HandValue.t * int

  let hand_matcher = Re2.create_exn "([AJKQT0-9]{5}) (\\d+)"
  exception Parse_error of string

  let parse_line line =
    match Parser.find_first_submatches hand_matcher line with
    | _ :: (Some hand) :: (Some bid) :: []  -> 
        let parsed_hand = String.to_list hand |> List.map ~f:Card.of_char in
        let hand_value = HandValue.calculate_from_cards parsed_hand in
        parsed_hand, hand_value, (Int.of_string bid)
    | _ -> raise (Parse_error line)

  let compare left right =
    let left_hand, left_val, _ = left in
    let right_hand, right_val, _ = right in
    let hand_difference = (HandValue.to_enum right_val) - (HandValue.to_enum left_val) in
    if hand_difference = 0 then
      Card.compare_list left_hand right_hand
    else hand_difference
end

let day7 () =
  let lines = In_channel.read_lines "./data/day7.txt" in
  let parsed = List.map lines ~f:Hand.parse_line in
  let sorted = List.sort ~compare:Hand.compare parsed in
  let values = List.map2_exn (List.rev sorted) (List.range 1 ((List.length sorted) + 1)) ~f:(fun row idx ->
    let _, _, bid = row in
    bid * idx
  ) in
  List.reduce_exn ~f:(+) values

