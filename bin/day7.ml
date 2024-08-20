let day7_inputs = Utils.read_lines "data/advent_7_data.txt"
let main = Printexc.record_backtrace true

type hand = { cards : string; bet : int }
type hand_type = FiveKind | FourKind | House | Tris | TwoPair | Pair | HighCard

let hand_type_of_int x =
  match x with
  | 25 -> FiveKind
  | 17 -> FourKind
  | 13 -> House
  | 11 -> Tris
  | 9 -> TwoPair
  | 7 -> Pair
  | 5 -> HighCard
  | _ -> failwith "Invalid hand type integer"

let int_of_card c =
  match c with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | '2' .. '9' -> int_of_char c - int_of_char '0'
  | _ -> failwith "Invalid card"

let int_of_card' c = match c with 'J' -> 1 | _ -> int_of_card c

let parse_hand line =
  let split = String.split_on_char ' ' line in
  let bet = int_of_string (List.nth split 1) in
  { cards = List.hd split; bet }

let hands = List.map (fun x -> parse_hand x) day7_inputs
let count_char_in c str = String.to_seq str |> Seq.fold_left (fun acc x -> if x = c then acc + 1 else acc) 0

(* Jokers will be 0 *)
let count_char_in' c str =
  if c = 'J' then 0 else String.to_seq str |> Seq.fold_left (fun acc x -> if x = c then acc + 1 else acc) 0

let apply_jokers seq =
  let rec aux s =
    if not (Seq.exists (fun x -> x = 0) s) then s
    else
      let highest = Seq.fold_left (fun acc x -> if x > acc then x else acc) 0 s in
      (* increment by i the first n occurences of high*)
      let rec inc s' high n i =
        match s' () with
        | Seq.Nil -> Seq.empty
        | Seq.Cons (x, tail) ->
            (* the max 2 is needed to solve the JJJJJ case*)
            if x = high && n <= 1 then Seq.cons (Int.max 2 (high + i)) tail
            else if x = high then Seq.cons (high + i) (inc tail high (n - 1) i)
            else Seq.cons x (inc tail high n i)
      in
      let new_seq = inc s highest highest 1 in
      (* increment the Joker (0) by setting it to the highest (after previous increment)*)
      let final_seq = inc new_seq 0 1 (highest + 1) in
      aux final_seq
  in
  aux seq

let hand_type count_char_fun h =
  String.to_seq h |> Seq.map (fun c -> count_char_fun c h) |> apply_jokers |> Seq.fold_left (fun acc x -> acc + x) 0

let ordering_func int_of_card_func count_char_func h1 h2 =
  let order_same_type_func h1 h2 =
    let rec compare_chars i =
      if i >= String.length h1 then 0
      else
        let result = int_of_card_func (String.get h1 i) - int_of_card_func (String.get h2 i) in
        if result <> 0 then result else compare_chars (i + 1)
    in
    compare_chars 0
  in
  let h1_value = hand_type count_char_func h1.cards in
  let h2_value = hand_type count_char_func h2.cards in
  let r = h1_value - h2_value in
  if r = 0 then order_same_type_func h1.cards h2.cards else r

let ordering h1 h2 = ordering_func int_of_card count_char_in h1 h2
let ordering' h1 h2 = ordering_func int_of_card' count_char_in' h1 h2
let compare = ordering { cards = "TJ3A4"; bet = 0 } { cards = "TJA63"; bet = 0 }
let () = Printf.printf " compare %d " compare

let execute () =
  let ordered_hands = List.sort ordering hands in
  let rec compute_winnings acc rank hands =
    match hands with [] -> acc | h :: rest -> compute_winnings (acc + (h.bet * rank)) (rank + 1) rest
  in
  compute_winnings 0 1 ordered_hands

let execute' () =
  let ordered_hands = List.sort ordering' hands in
  let rec compute_winnings acc rank hands =
    match hands with [] -> acc | h :: rest -> compute_winnings (acc + (h.bet * rank)) (rank + 1) rest
  in
  compute_winnings 0 1 ordered_hands
