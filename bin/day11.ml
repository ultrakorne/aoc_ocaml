let day11_inputs = Utils.read_lines "data/advent_11_data.txt"

type coord = { x : int; y : int }

module CoordMap = Map.Make (struct
  type t = coord

  let compare = compare
end)

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

let expand_space gal =
  let rec expand get_coord set_coord acc c exp g =
    match g with
    | [] -> acc
    | g' :: tail ->
        let coord = get_coord g' in
        let exp' = exp + if coord + exp <= c + 1 then 0 else coord + exp - c - 1 in
        let g' = set_coord g' (coord + exp') in
        let coord = get_coord g' in
        expand get_coord set_coord (g' :: acc) coord exp' tail
  in

  let expand_x = expand (fun c -> c.x) (fun c x -> { c with x }) [] 0 0 in
  let expand_y = expand (fun c -> c.y) (fun c y -> { c with y }) [] 0 0 in
  gal |> List.sort (fun c1 c2 -> c1.x - c2.x) |> expand_x |> List.sort (fun c1 c2 -> c1.y - c2.y) |> expand_y

let execute () =
  let galaxy_list = parse_input day11_inputs in
  Printf.printf "\ngalaxies %d" (List.length galaxy_list);
  (* List.iter (fun c -> Printf.printf "\ncoord %d,%d" c.x c.y) galaxy_list; *)
  let expanded_galaxy_list = expand_space galaxy_list in
  List.iter (fun c -> Printf.printf "\ncoord %d,%d" c.x c.y) expanded_galaxy_list;
  0

let execute' () = 0
