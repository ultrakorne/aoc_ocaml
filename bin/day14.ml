let day14_inputs = Utils.read_lines "data/advent_14_data_test.txt"

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
  match dir with
  | North -> coord_to_check.y < 0
  | West -> coord_to_check.x < 0
  | East -> coord_to_check.x >= dish_size.x
  | South -> coord_to_check.y >= dish_size.y

let falling_by dir i =
  match dir with
  | North -> { x = 0; y = -1 * i }
  | West -> { x = -1 * i; y = 0 }
  | East -> { x = 1 * i; y = 0 }
  | South -> { x = 0; y = 1 * i }

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

let print_dish dish =
  let rec aux x y =
    if x >= dish_size.x then
      let () = Printf.printf "\n" in
      aux 0 (y + 1)
    else if y >= dish_size.y then Printf.printf "\n"
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
  Printf.printf "\n";
  aux 0 0

let roll_direction dir dish_map = Dish.fold (fun k c acc -> roll acc (k, c) dir) dish_map dish_map

let full_cycle dish_map =
  roll_direction North dish_map |> roll_direction West |> roll_direction South |> roll_direction East

let execute () =
  Printf.printf "dish size w %d, h %d \n" dish_size.x dish_size.y;
  Printf.printf "lenght %d \n" (List.length dish_list);
  (* print_dish dish dish_size.x dish_size.y; *)
  let dish_north = roll_direction North dish in
  Dish.fold (fun k c acc -> if c = 'O' then acc + rock_load North k else acc) dish_north 0

let cycle_n_times n =
  let rec aux acc i dish =
    if i >= n then List.rev acc
    else
      let cycle = full_cycle dish in
      let result = Dish.fold (fun k c acc -> if c = 'O' then acc + rock_load North k else acc) cycle 0 in
      aux (result :: acc) (i + 1) cycle
  in
  aux [] 0 dish

module IntMap = Map.Make (Int)

type occurrence = { amount : int; first_occ : int; second_occ : int }

let find_pattern list =
  let rec aux list map i =
    match list with
    | [] -> map
    | l :: rest ->
        let new_map =
          IntMap.update l
            (function
              | None -> Some { amount = 1; first_occ = i; second_occ = 0 }
              | Some o when o.second_occ = 0 -> Some { o with amount = o.amount + 1; second_occ = i }
              | Some o -> Some { o with amount = o.amount + 1 })
            map
        in
        aux rest new_map (i + 1)
  in

  let result = aux list IntMap.empty 1 in
  IntMap.filter (fun _ v -> v.amount > 1) result

let execute' () =
  Printf.printf "\n";
  let t = Sys.time () in
  (* let cycle = full_cycle dish in *)
  (* let result = Dish.fold (fun k c acc -> if c = 'O' then acc + rock_load North k else acc) cycle 0 in *)
  let results = cycle_n_times 250 in
  List.iter (fun x -> Printf.printf "%d " x) results;
  let pattern = find_pattern results in
  let pattern_list =
    IntMap.to_list pattern |> List.sort (fun (_, v) (_, v1) -> v.first_occ- v1.first_occ)
  in
  List.iter (fun (k, v) -> Printf.printf "\nkey:%d -> %d:i=%d:s=%d |" k v.amount v.first_occ v.second_occ) pattern_list;

  Printf.printf "\nExecution time: %fs\n" (Sys.time () -. t);
  0
