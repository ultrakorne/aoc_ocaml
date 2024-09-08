let day15_inputs = Utils.read_lines "data/advent_15_data.txt"
let parse_input input = String.split_on_char ',' input

type seq_step = { label : string; op : char; focal_length : int }

module IntMap = Map.Make (Int)

let parse_input' input =
  let rec aux steps acc =
    match steps with
    | [] -> List.rev acc
    | s :: rest ->
        let minus = String.split_on_char '-' s in
        if List.length minus > 1 then aux rest ({ label = List.hd minus; op = '-'; focal_length = 0 } :: acc)
        else
          let equal = String.split_on_char '=' s in
          aux rest ({ label = List.hd equal; op = '='; focal_length = int_of_string (List.rev equal |> List.hd) } :: acc)
  in
  let split = parse_input input in
  aux split []

let hash_string str =
  let rec aux str acc i =
    if i >= String.length str then acc
    else
      let value = ((str.[i] |> int_of_char) + acc) * 17 mod 256 in
      aux str value (i + 1)
  in
  aux str 0 0

let compute_hash sequence =
  let rec aux seq acc =
    match seq with
    | [] -> acc
    | s :: rest ->
        let hash = hash_string s in
        aux rest (acc + hash)
  in
  aux sequence 0

let add_in_boxes steps =
  let add_step step boxes =
    let update_inside_box box_lst step =
      if step.op = '-' then List.filter (fun s -> s.label <> step.label) box_lst
      else if List.find_opt (fun s -> s.label = step.label) box_lst = None then box_lst @ [ step ]
      else List.map (fun s -> if s.label = step.label then step else s) box_lst
    in

    let hash = hash_string step.label in
    IntMap.update hash
      (function None -> if step.op = '-' then None else Some [ step ] | Some h -> Some (update_inside_box h step))
      boxes
  in
  let rec aux steps boxes =
    match steps with
    | [] -> boxes
    | s :: rest ->
        let new_boxes = add_step s boxes in
        aux rest new_boxes
  in
  aux steps IntMap.empty |> IntMap.filter (fun _ v -> List.length v > 0)

let compute_score boxes =
  let rec box_score box box_value acc i =
    match box with
    | [] -> acc
    | lens :: rest ->
        let new_acc = acc + (box_value * i * lens.focal_length) in
        box_score rest box_value new_acc (i + 1)
  in
  IntMap.fold (fun k v acc -> acc + box_score v (k + 1) 0 1) boxes 0

let execute () = parse_input (List.hd day15_inputs) |> compute_hash
let execute' () = parse_input' (List.hd day15_inputs) |> add_in_boxes |> compute_score
