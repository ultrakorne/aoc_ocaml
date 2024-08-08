(*
  The Elf would first like to know which games would have been possible if the bag contained only
  12 red cubes,
  13 green cubes
  14 blue cubes?
  What is the sum of the IDs of those games
*)

let day2_inputs = Utils.read_lines "data/advent_2_data.txt"
let test_line = List.nth day2_inputs 0

(* not really needed but wanted to check out variants *)
type cube_color = Red | Green | Blue

let string_of_color = function
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"

let color_of_string = function
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | _ -> failwith "Invalid color"

let first_bag_content = function Red -> 12 | Green -> 13 | Blue -> 14

(*Check if a game segment. a single segment is for example 1 red, 5 blue*)
let is_subgame_invalid game =
  (* Printf.printf "Game: %s\n" game; *)
  let split = String.split_on_char ',' game in
  let cube_count_invalid game_cubes =
    let cube = String.trim game_cubes |> String.split_on_char ' ' in
    let count = List.hd cube |> int_of_string in
    let color = List.nth cube 1 |> color_of_string in
    (* Printf.printf "Cube Count: %d\n" count; *)
    (* Printf.printf "Cube Color: %s\n" (string_of_color color); *)
    first_bag_content color < count
  in
  let invalid = List.exists (fun g -> cube_count_invalid g) split in
  invalid

(* return 0 if game is invalid, the game id otherwise *)
let process_game game =
  let split = String.split_on_char ':' game in
  let game_id =
    List.hd split |> String.split_on_char ' ' |> List.rev |> List.hd
    |> int_of_string
  in
  let game_content = List.tl split |> List.hd |> String.split_on_char ';' in
  let invalid = List.exists (fun x -> is_subgame_invalid x) game_content in
  (* Printf.printf "Game ID: %d\n" game_id; *)
  (* Printf.printf "Game is invalid: %b\n" invalid; *)
  if invalid then 0 else game_id

let execute_day_2 =
  List.fold_left (fun acc x -> acc + process_game x) 0 day2_inputs

let parse_test = process_game test_line
let () = Printf.printf "\nTest Result: %d\n" parse_test
