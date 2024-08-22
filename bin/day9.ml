let day9_inputs = Utils.read_lines "data/advent_9_data.txt"
let test_line = List.nth day9_inputs 2
let parse_line line = String.split_on_char ' ' line |> List.map int_of_string
let test_result = parse_line test_line
let () = List.iter (Printf.printf "\n init: %d ") test_result

let find_next_num list =
  let rec aux lst =
    let rec sub lst sub_lst =
      match lst with l1 :: l2 :: tail -> sub (l2 :: tail) (sub_lst @ [ l2 - l1 ]) | _ -> sub_lst
    in

    if List.exists (fun x -> x <> 0) lst then
      let prev_sub = sub lst [] in
      let res = aux prev_sub in
      let return = res + (List.rev prev_sub |> List.hd) in
      return
    else 0
  in
  aux list + (List.rev list |> List.hd)

let find_prev_num list =
  let rec aux lst =
    let rec sub lst sub_lst =
      match lst with l1 :: l2 :: tail -> sub (l2 :: tail) (sub_lst @ [ l2 - l1 ]) | _ -> sub_lst
    in

    if List.exists (fun x -> x <> 0) lst then
      let prev_sub = sub lst [] in
      let res = aux prev_sub in
      let return = List.hd prev_sub - res in
      return
    else 0
  in
  List.hd list - aux list

let execute () = List.fold_left (fun acc x -> acc + (parse_line x |> find_next_num)) 0 day9_inputs

let execute' () =
  List.fold_left (fun acc x -> acc + (parse_line x |> find_prev_num)) 0 day9_inputs
