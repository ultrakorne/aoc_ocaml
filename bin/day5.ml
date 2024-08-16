let day5_inputs = Utils.read_lines "data/advent_5_data.txt"

type map_entry = { dest : int; source : int; length : int }
type single_map = { mappings : map_entry list }
type mappings = { map : single_map list }

let parse_input input =
  let seeds =
    List.hd input |> String.split_on_char ':' |> List.tl |> List.hd |> String.trim |> String.split_on_char ' '
    |> List.map int_of_string
  in
  let maps = List.tl input in

  let rec parse_map map acc =
    match map with
    | [] -> (acc, [])
    | x :: xs ->
        if String.trim x = "" then (acc, xs)
        else
          let split = String.split_on_char ' ' x in
          let dest = List.hd split |> int_of_string in
          let source = List.nth split 1 |> int_of_string in
          let length = List.nth split 2 |> int_of_string in
          let entry = { dest; source; length } in
          parse_map xs (entry :: acc)
  in

  let rec parse_all_maps map =
    match map with
    | [] -> []
    | x :: xs ->
        if String.contains x ':' then parse_all_maps xs
        else
          let parsed_map, remain_maps = parse_map xs [] in
          let single_map = { mappings = parsed_map } in
          single_map :: parse_all_maps remain_maps
  in

  let result = parse_all_maps maps in
  (seeds, result)

let seeds, all_maps = parse_input day5_inputs
let () = List.iter (Printf.printf "\n seed num %d") seeds
let execute = 0
