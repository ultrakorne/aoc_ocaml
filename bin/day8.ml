let day8_inputs = Utils.read_lines "data/advent_8_data.txt"
let lr_instructions = List.hd day8_inputs

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

let next_node curr dir =
  let curr_net =
    match StringMap.find_opt curr network with
    | Some c -> c
    | None -> failwith (Printf.sprintf "Key |%s| not found in the map" curr)
  in
  match dir with 'L' -> curr_net.l | 'R' -> curr_net.r | _ -> failwith "invalid direction only L and R supported"

let traverse_network start dir_seq =
  let rec aux curr seq step =
    if curr = "ZZZ" then step
    else
      match seq () with
      | Seq.Nil -> aux curr dir_seq step
      | Seq.Cons (x, tail) ->
          let next_node = next_node curr x in
          aux next_node tail (step + 1)
  in
  aux start dir_seq 0

let traverse_network' start dir_seq =
  let rec aux curr seq step =
    if String.ends_with ~suffix:"Z" curr then step
    else
      match seq () with
      | Seq.Nil -> aux curr dir_seq step
      | Seq.Cons (x, tail) ->
          let next_node = next_node curr x in
          aux next_node tail (step + 1)
  in
  aux start dir_seq 0

let rec lcm i arr =
  let rec gcd a b = if a = 0 then b else gcd (b mod a) a in
  if i = Array.length arr - 1 then arr.(i)
  else
    let b = lcm (i + 1) arr in
    arr.(i) * b / gcd arr.(i) b

let execute () = String.to_seq lr_instructions |> traverse_network "AAA"

let execute' () =
  let seq = String.to_seq lr_instructions in
  StringMap.filter (fun k _ -> String.ends_with ~suffix:"A" k) network
  |> StringMap.to_list |> List.map fst
  |> List.map (fun x -> traverse_network' x seq)
  |> Array.of_list
  |> lcm 0

