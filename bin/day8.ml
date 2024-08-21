let day8_inputs = Utils.read_lines "data/advent_8_data.txt"
let lr_instructions = List.hd day8_inputs
(* let list_directions = List.tl day8_inputs |> List.tl *)

module StringMap = Map.Make (String)

type net_dir = { l : string; r : string }

let parse_direction line =
  let regex = Str.regexp {|\([A-Z]+\) = [(]\([A-Z]+\), \([A-Z]+\)|} in
  if Str.string_match regex line 0 then
    let from = Str.matched_group 1 line in
    let left = Str.matched_group 2 line in
    let right = Str.matched_group 3 line in
    Some (from, { l = left; r = right })
  else None

let directions = List.tl day8_inputs |> List.filter_map parse_direction
let network = StringMap.of_list directions

let find_key key =
  (* let () = StringMap.iter (fun k _ -> if k = key then Printf.printf "Map contains key |%s|\n%!" k) network in *)
  let curr_net = StringMap.find_opt key network in
  let curr_net' =
    match curr_net with Some c -> c | None -> failwith (Printf.sprintf "Key |%s| not found in the map" key)
  in
  curr_net'

let next_node curr dir =
  let curr_net = find_key curr in
  match dir with 'L' -> curr_net.l | 'R' -> curr_net.r | _ -> failwith "invalid direction only L and R supported"

let traverse_network dir_seq =
  let rec aux curr seq step =
    if curr = "ZZZ" then step
    else
      match seq () with
      | Seq.Nil -> aux curr dir_seq step
      | Seq.Cons (x, tail) ->
          let next_node = next_node curr x in
          aux next_node tail (step + 1)
  in
  aux "AAA" dir_seq 0

let execute () =
  (* let () = StringMap.iter (fun key value -> Printf.printf "\n%s %s %s " key value.l value.r) network in *)
  let result = String.to_seq lr_instructions |> traverse_network in
  result

let execute' () = 0
