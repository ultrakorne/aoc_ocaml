let day14_inputs = Utils.read_lines "data/advent_14_data.txt"

type coord = { x : int; y : int }
type direction = North | East | South | West

module Dish = Map.Make (struct
  type t = coord

  let compare = compare
end)

let parse_input input =
  let rec parse_row seq x y lst =
    match seq () with
    | Seq.Nil -> (lst, x)
    | Seq.Cons (s, tail) ->
        let new_lst = if s != '.' then ({ x; y }, s) :: lst else lst in
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

let dish_list, dish_size = parse_input day14_inputs
let dish = Dish.of_list dish_list
let sum_coord c1 c2 = { x = c1.x + c2.x; y = c1.y + c2.y }

let check_out_bound coord_to_check dir =
  match dir with North -> coord_to_check.y < 0 | _ -> failwith "direction not supported"

let falling_by dir i = match dir with North -> { x = 0; y = -1 * i } | _ -> failwith "direction not supported"

let rock_load dir coord =
  match dir with North -> dish_size.y - coord.y | _ -> failwith "direction not supported in rock_load"

let roll dish d dir =
  let rec find_highest_empty acc coord dir i =
    let shift = falling_by dir i in
    let coord_to_check = sum_coord coord shift in
    let reached_end = check_out_bound coord_to_check dir in
    if reached_end then acc
    else
      let symbol_at = Dish.find_opt coord_to_check dish in
      match symbol_at with
      | None -> find_highest_empty coord_to_check coord dir (i + 1)
      | Some '#' -> acc
      | Some 'O' -> find_highest_empty acc coord dir (i + 1)
      | _ -> failwith "this should never happen"
  in
  let symbol = snd d in
  if symbol = '#' then dish
  else
    let coord = fst d in
    let new_rock_position = find_highest_empty coord coord dir 1 in
    if new_rock_position = coord then dish
    else
      let new_dish = Dish.remove coord dish in
      let new_dish = Dish.add new_rock_position 'O' new_dish in
      new_dish

let print_dish dish w h =
  let rec aux x y =
    if x >= w then
      let () = Printf.printf "\n" in
      aux 0 (y + 1)
    else if y >= h then Printf.printf "\n"
    else
      let sym = Dish.find_opt { x; y } dish in
      match sym with
      | None ->
          Printf.printf ".";
          aux (x + 1) y
      | Some c ->
          Printf.printf "%c" c;
          aux (x + 1) y
  in

  aux 0 0

let roll_direction dish_map dish_list dir =
  let rec aux acc dish =
    match dish with
    | [] -> acc
    | d :: rest ->
        let new_acc = roll acc d dir in
        aux new_acc rest
  in
  aux dish_map dish_list

let execute () =
  Printf.printf "dish size w %d, h %d \n" dish_size.x dish_size.y;
  Printf.printf "lenght %d \n" (List.length dish_list);
  (* print_dish dish dish_size.x dish_size.y; *)
  let dish_north = roll_direction dish dish_list North in
  Dish.fold (fun k c acc -> if c = 'O' then acc + rock_load North k else acc) dish_north 0
