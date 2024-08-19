let day6_inputs = Utils.read_lines "data/advent_6_data.txt"

let parse_input_line line =
  let split = String.split_on_char ':' line |> List.tl |> List.hd in
  let re = Str.regexp " +" in
  Str.split re split |> List.map (fun x -> int_of_string x)

let parse_input_line' line =
  let re = Str.regexp "[^0-9]+" in
  Str.global_replace re "" line |> int_of_string

let times = parse_input_line (List.nth day6_inputs 0)
let time' = parse_input_line' (List.nth day6_inputs 0)
let distances = parse_input_line (List.nth day6_inputs 1)
let distance' = parse_input_line' (List.nth day6_inputs 1)

let ways_to_beat time distance =
  let rec release_at_time t acc =
    if t >= time then acc
    else
      let mm_per_ms = t in
      let time_left = time - t in
      let t_distance = mm_per_ms * time_left in
      let beat_record = if t_distance > distance then 1 else 0 in
      release_at_time (t + 1) (acc + beat_record)
  in
  release_at_time 1 0

let execute () = List.map2 (fun t d -> ways_to_beat t d) times distances |> List.fold_left (fun acc x -> acc * x) 1
let execute' () = ways_to_beat time' distance'
