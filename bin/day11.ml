let day11_inputs = Utils.read_lines "data/advent_11_data.txt"

type coord = { x : int; y : int }

let parse_input input =
  let rec parse_line acc line row col =
    if col >= String.length line then acc
    else
      match String.get line col with
      | '.' -> parse_line acc line row (col + 1)
      | '#' ->
          let new_acc = { x = col; y = row } :: acc in
          parse_line new_acc line row (col + 1)
      | _ -> failwith "only char valid are . and #"
  in
  let rec aux acc lines row =
    match lines with
    | [] -> acc
    | line :: tail ->
        let line_result = parse_line [] line row 0 in
        aux (line_result @ acc) tail (row + 1)
  in
  aux [] input 0

let expand_space amount galaxies =
  let rec expand get_coord set_coord acc c exp galaxies =
    match galaxies with
    | [] -> acc
    | g :: tail ->
        let coord = get_coord g in
        let exp' = exp + if coord + exp <= c + 1 then 0 else (coord + exp - c - 1) * (amount - 1) in
        let g = set_coord g (coord + exp') in
        let coord = get_coord g in
        expand get_coord set_coord (g :: acc) coord exp' tail
  in

  let expand_x = expand (fun c -> c.x) (fun c x -> { c with x }) [] 0 0 in
  let expand_y = expand (fun c -> c.y) (fun c y -> { c with y }) [] 0 0 in
  let sort_x c1 c2 = c1.x - c2.x in
  let sort_y c1 c2 = c1.y - c2.y in
  galaxies |> List.sort sort_x |> expand_x |> List.sort sort_y |> expand_y

let galaxy_distances galaxies =
  let distance g g' = Int.abs (g.x - g'.x) + Int.abs (g.y - g'.y) in
  let rec all_distance acc g rest =
    match rest with
    | [] -> acc
    | g' :: tail ->
        let d = distance g g' in
        all_distance (acc + d) g tail
  in
  let rec aux acc gal =
    match gal with
    | [] -> acc
    | g :: tail ->
        let dis = all_distance 0 g tail in
        aux (dis + acc) tail
  in
  aux 0 galaxies

let galaxy_list = parse_input day11_inputs
let execute () = expand_space 2 galaxy_list |> galaxy_distances
let execute' () = expand_space 1_000_000 galaxy_list |> galaxy_distances
