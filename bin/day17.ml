(* open Minttea *)

let day17_inputs = Utils.read_lines "data/advent_17_data_test.txt"

type coord = { x : int; y : int }
type node = { coord : coord; dimension : string }
type node_info = { node : node; weight : int; path : coord list }

type heading = Right | Down | Left | Up
let string_of_heading = function
  | Right -> ">"
  | Down -> "v"
  | Left -> "<"
  | Up -> "^"

module Grid = Map.Make (struct
  type t = coord

  let compare = compare
end)

let parse_input input =
  let digit_of_char char = int_of_char char - int_of_char '0' in
  let rec parse_row seq x y lst =
    match seq () with
    | Seq.Nil -> (lst, x)
    | Seq.Cons (s, tail) ->
        let new_lst = if s != '.' then ({ x; y }, digit_of_char s) :: lst else lst in
        parse_row tail (x + 1) y new_lst
  in
  let rec aux line y lst width =
    match line with
    | [] -> (lst, { x = width; y })
    | l :: tail ->
        let seq = String.to_seq l in
        let row, x = parse_row seq 0 y [] in
        aux tail (y + 1) (List.rev_append row lst) (Int.max x width)
  in
  aux input 0 [] 0

let coord_of_heading coord heading =
  match heading with
  | Right -> { coord with x = coord.x + 1 }
  | Left -> { coord with x = coord.x - 1 }
  | Down -> { coord with y = coord.y + 1 }
  | Up -> { coord with y = coord.y - 1 }

let last_heading path =
  let rec aux p acc =
    if List.length acc >= 3 then acc else 
    match p with
    |a :: b :: rest -> let heading = match a.x - b.x, a.y - b.y with
      | 1, 0 -> Right
      | -1, 0 -> Left
      | 0, 1 -> Down
      | 0, -1 -> Up
      | _ -> failwith "Invalid path direction"
    in
    aux (b :: rest) (heading :: acc)
    | _ -> acc
  in
  aux path []
let next_valid_headings headings =
  let no_backward a =
  match a with
  | Right -> [Right; Down; Up]
  | Down -> [Down; Right; Left]
  | Left -> [Left; Down; Up]
  | Up -> [Up; Right; Left]
  in
  match headings with
  | [] -> [Right; Down; Left; Up]
  | a :: b :: c :: _ -> if a = b && b = c then no_backward a |> List.filter (fun x -> x <> a) else no_backward a
  | a :: _ -> no_backward a
  
let node_of_coord coord last_headings =
  if List.length last_headings = 0 then { coord; dimension = "*" }
  else
  let rec aux headings acc hd_headings =
    if String.length acc >= 3 then acc else
    match headings with
    | [] -> acc
    | h :: rest -> 
      if h <> hd_headings then acc else aux rest ( (string_of_heading h) ^ acc) hd_headings
  in
  let dimension_str = aux last_headings "" (List.hd last_headings) in
  {coord; dimension = dimension_str}

  let update_prio_list prio_list nodes_weighted path =
  List.fold_left( fun acc x -> 
    let node = fst x in
    let weight = snd x in
    if List.exists (fun y -> y.node = node && y.weight > weight) prio_list then 
      List.map (fun y -> if y.node = node then {node;weight;path=path} else y) prio_list else {node; weight; path = path} :: acc 
    ) prio_list nodes_weighted

let rec get_next_node prio_list visited =
    match prio_list with
    | [] -> failwith "cannot find any more nodes in prio list that are not visited"
    | hd :: rest -> if List.exists (fun x -> x = hd.node) visited then get_next_node rest visited else hd
    
let pathfind_step grid node weight path visited prio_list =
  let coord = node.coord in
  let new_path = coord :: path in
  let last_headings = last_heading new_path in
  (* let node = node_of_coord coord last_headings in *)
  let next_headings = next_valid_headings last_headings in
  let connected_nodes_weighted = List.filter_map (fun x -> 
    let c = coord_of_heading coord x in 
    let n = node_of_coord c ( x :: last_headings) in
    Grid.find_opt c grid |> Option.map (fun y -> (n, y + weight))
    ) next_headings in
  let visited = node :: visited in

  let new_prio_list = update_prio_list prio_list connected_nodes_weighted new_path in
  let new_prio_list = List.sort (fun a b -> compare a.weight b.weight) new_prio_list in
  let next_node_info = get_next_node new_prio_list visited in
  pathfind_step grid next_node_info.node next_node_info.weight next_node_info.path visited new_prio_list 
    



let grid_list, grid_size = parse_input day17_inputs
let grid = Grid.of_list grid_list
let execute () = 0
