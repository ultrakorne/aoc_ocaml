let day5_inputs = Utils.read_lines "data/advent_5_data.txt"
let main = Printexc.record_backtrace true

type map_entry = { dest : int; source : int; length : int }
type single_map = { mappings : map_entry list }
(* type mappings = { map : single_map list }  *)

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
        (*stop on empty line, returning the rest of the lines*)
        if String.trim x = "" then (acc, xs)
        else
          let () = Printf.printf "\n parsing map entry %s" x in
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
        let () = Printf.printf "\n parsing map %s" x in
        (*skipping lines that are empty and with :, since they indicate a beginning of a map*)
        if String.contains x ':' || String.trim x = "" then parse_all_maps xs
        else
          let parsed_map, remain_maps = parse_map (x :: xs) [] in
          let single_map = { mappings = List.rev parsed_map } in
          single_map :: parse_all_maps remain_maps
  in

  let result = parse_all_maps maps in
  (seeds, result)

let seeds, all_maps = parse_input day5_inputs
let () = List.iter (Printf.printf "\n seed num %d") seeds
let () = Printf.printf "\n num maps %d" (List.length all_maps)
let () = List.iter (Printf.printf "\n dest first map %d") (List.map (fun x -> x.dest) (List.nth all_maps 1).mappings)

let apply_map value map =
  List.find_opt (fun x -> value >= x.source && value < x.source + x.length) map
  |> Option.map (fun x -> value + x.dest - x.source)

let map_seed seed maps =
  List.fold_left
    (fun acc map ->
      let () = Printf.printf "\n applying map to num %d" acc in
      let value = apply_map acc map.mappings in
      match value with Some x -> x | None -> acc)
    seed maps

let execute =
  let min_location = List.map (fun x -> map_seed x all_maps) seeds |> List.fold_left (fun acc x -> if x < acc then x else acc) Int.max_int in

  let seed_loc = map_seed 79 all_maps in
  let () = Printf.printf "\n seed loc %d" seed_loc in
  let () = Printf.printf "\n min location %d" min_location in
  min_location
