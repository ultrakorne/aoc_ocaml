let day9_inputs = Utils.read_lines "data/advent_9_data.txt"
let parse_line line = String.split_on_char ' ' line |> List.map int_of_string

let predict_num num_fun list =
  let rec aux lst =
    let rec sub lst sub_lst =
      match lst with l1 :: l2 :: tail -> sub (l2 :: tail) (sub_lst @ [ l2 - l1 ]) | _ -> sub_lst
    in
    if List.exists (fun x -> x <> 0) lst then
      let prev_sub = sub lst [] in
      aux prev_sub |> num_fun prev_sub
    else 0
  in
  aux list |> num_fun list

let day9 num_fun = List.fold_left (fun acc x -> acc + (parse_line x |> predict_num num_fun)) 0 day9_inputs
let execute () = (fun lst n -> n + (List.rev lst |> List.hd)) |> day9
let execute' () = (fun lst n -> List.hd lst - n) |> day9