let day10_inputs = Utils.read_lines "data/advent_10_data_test.txt"

type coord = { x : int; y : int }

module Matrix = Map.Make (struct
  type t = coord

  let compare = compare
end)

let parse_input input =
  let rec parse_row seq x y mtx_lst =
    match seq () with
    | Seq.Nil -> mtx_lst
    | Seq.Cons (s, tail) ->
        let new_lst = ({ x; y }, s) :: mtx_lst in
        parse_row tail x (y + 1) new_lst
  in
  let rec aux line x mtx_lst =
    match line with
    | [] -> mtx_lst
    | l :: tail ->
        let seq = String.to_seq l in
        let row = parse_row seq x 0 mtx_lst in
        aux tail (x + 1) (row @ mtx_lst)
  in
  aux input 0 []

let matrix_list = parse_input day10_inputs
let matrix = Matrix.of_list matrix_list

let execute () =
  let coord1 = Matrix.find { x = 2; y = 1 } matrix in
  let () = Printf.printf "\nvalue %c" coord1 in
  0

let execute' () = 0
