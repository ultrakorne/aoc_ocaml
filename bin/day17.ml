(* open Minttea *)

let day17_inputs = Utils.read_lines "data/advent_17_data_test.txt"

type coord = { x : int; y : int }
type node = { coord : coord; dimension : string }
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

let node_of_coord coord last_headings =
  if List.length last_headings = 0 then { coord; dimension = "*" }
  else
  let rec aux headings acc hd_headings =
    match headings with
    | [] -> acc
    | h :: rest -> 
      if h <> hd_headings then acc else aux rest ( (string_of_heading h) ^ acc) hd_headings
  in
  let dimension_str = aux last_headings "" (List.hd last_headings) in
  {coord; dimension = dimension_str}

let pathfind_step grid coord path visited prio_list =
  (* visiting coord *)
  let new_path = coord :: path in
  let last_headings = last_heading new_path in
  let node = node_of_coord coord last_headings in
  let visited = node :: visited in

let grid_list, grid_size = parse_input day17_inputs
let grid = Grid.of_list grid_list
let execute () = 0
