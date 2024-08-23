let day10_inputs = Utils.read_lines "data/advent_10_data_test.txt"

type coord = { x : int; y : int }

module Matrix = Map.Make (struct
  type t = coord

  let compare = compare
end)

(* return a list of (coord, char) as fst and snd the coord of the starting position 'S'*)
let parse_input input =
  let rec parse_row seq x y mtx_lst start =
    match seq () with
    | Seq.Nil -> (mtx_lst, start)
    | Seq.Cons (s, tail) ->
        let start_coord = if start <> None then start else if s = 'S' then Some { x; y } else None in
        let new_lst = ({ x; y }, s) :: mtx_lst in
        parse_row tail x (y + 1) new_lst start_coord
  in
  let rec aux line x mtx_lst start =
    match line with
    | [] -> (mtx_lst, start)
    | l :: tail ->
        let seq = String.to_seq l in
        let row, start_coord = parse_row seq x 0 mtx_lst start in
        aux tail (x + 1) (row @ mtx_lst) start_coord
  in
  aux input 0 [] None

let matrix_list, start = parse_input day10_inputs
let matrix = Matrix.of_list matrix_list

let execute () =
  let coord1 = Matrix.find { x = 2; y = 1 } matrix in
  match start with
  | Some start_coord ->
      let () = Printf.printf "\nstart x:%d y:%d" start_coord.x start_coord.y in
      let () = Printf.printf "\nvalue %c" coord1 in
      0
  | None ->
      let () = Printf.printf "no start character S found" in
      0

let execute' () = 0
