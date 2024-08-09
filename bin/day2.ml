(*
  The Elf would first like to know which games would have been possible if the bag contained only
  12 red cubes,
  13 green cubes
  14 blue cubes?
  What is the sum of the IDs of those games
*)

let day2_inputs = Utils.read_lines "data/advent_2_data.txt"

type subgame = { red : int; green : int; blue : int }
let max_bag = { red = 12; green = 13; blue = 14 }

(*Convert a string to a subgame*)
let subgame_of_string s =
  (* Printf.printf "Subgame: %s\n" s; *)
  let split = String.split_on_char ',' s in
  let tuple_of_cube game_cubes =
    let cube = String.trim game_cubes |> String.split_on_char ' ' in
    let count = List.hd cube |> int_of_string in
    let color = List.nth cube 1 in
    (color, count)
  in
  let sg =
    List.fold_left
      (fun acc s ->
        let color, count = tuple_of_cube s in
        match color with
        | "red" -> { acc with red = count }
        | "green" -> { acc with green = count }
        | "blue" -> { acc with blue = count }
        | _ -> failwith "Invalid color")
      { red = 0; green = 0; blue = 0 }
      split
  in
  sg

(*Check if a game segment. a single segment is for example 1 red, 5 blue*)
let is_subgame_invalid game =
  let sg = subgame_of_string game in
  sg.red > max_bag.red || sg.green > max_bag.green || sg.blue > max_bag.blue

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
  if invalid then 0 else game_id

(* Process game for second star *)
let process_game' game =
  let split = String.split_on_char ':' game in
  let game_content = List.tl split |> List.hd |> String.split_on_char ';' in
  let max_bag =
    List.fold_left
      (fun acc s ->
        let sg = subgame_of_string s in
        {
          red = Int.max sg.red acc.red;
          green = Int.max sg.green acc.green;
          blue = Int.max sg.blue acc.blue;
        })
      { red = 0; green = 0; blue = 0 }
      game_content
  in
  max_bag.red * max_bag.green * max_bag.blue

let execute_day_2 =
  List.fold_left (fun acc x -> acc + process_game x) 0 day2_inputs

let execute_day_2' =
  List.fold_left (fun acc x -> acc + process_game' x) 0 day2_inputs
