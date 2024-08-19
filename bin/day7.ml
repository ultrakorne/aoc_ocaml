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

let parse_hand line =
  let split = String.split_on_char ' ' line in
  let bet = int_of_string (List.nth split 1) in
  { cards = List.hd split; bet }

let hands = List.map (fun x -> parse_hand x) day7_inputs
let count_char_contained_in c str = Seq.fold_left (fun acc x -> if x = c then acc + 1 else acc) 0 (String.to_seq str)

let hand_type h =
  let occurrences = List.of_seq (Seq.map (fun c -> count_char_contained_in c h) (String.to_seq h)) in
  List.fold_left (fun acc x -> acc + x) 0 occurrences


let ordering_func h1 h2 =
  let order_same_type_func h1 h2 =
    let rec compare_chars i =
      if i >= String.length h1 then 0
      else
        let result = int_of_card (String.get h1 i) - int_of_card (String.get h2 i) in
        if result <> 0 then result else compare_chars (i + 1)
    in
    compare_chars 0
  in
  let h1_value = hand_type h1.cards in
  let h2_value = hand_type h2.cards in
  let r = h1_value - h2_value in
  if r = 0 then order_same_type_func h1.cards h2.cards else r

let compare = ordering_func { cards = "TJ3A4"; bet = 0 } { cards = "TJA63"; bet = 0 }
let () = Printf.printf " compare %d " compare

let execute () =
  let ordered_hands = List.sort ordering_func hands in
  let () = Printf.printf "\n t cards rank 1 %s" (List.nth ordered_hands 0).cards in
  let () = Printf.printf "\n t cards rank 2 %s" (List.nth ordered_hands 1).cards in
  let () = Printf.printf "\n t cards rank 3 %s" (List.nth ordered_hands 2).cards in
  let () = Printf.printf "\n t cards rank 4 %s" (List.nth ordered_hands 3).cards in
  let () = Printf.printf "\n t cards rank 5 %s" (List.nth ordered_hands 4).cards in
  let rec compute_winnings acc rank hands =
    match hands with [] -> acc | h :: rest -> compute_winnings (acc + (h.bet * rank)) (rank + 1) rest
  in
  compute_winnings 0 1 ordered_hands

let execute' () = 0
