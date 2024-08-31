let day12_inputs = Utils.read_lines "data/advent_12_data.txt"

type spring = { dmg_mask : int; op_mask : int; unkn_mask : int; length : int; damage_group : int list }

let binary_string_of_int n =
  let rec aux n acc = if n = 0 then acc else aux (n lsr 1) (string_of_int (n land 1) ^ acc) in
  if n = 0 then "0" else aux n ""

let parse_input input =
  let rec parse_spring acc spring_input i =
    if i >= String.length spring_input then acc
    else
      let new_acc =
        match spring_input.[i] with
        | '.' ->
            let new_mask = acc.op_mask lsl 1 in
            let new_mask = new_mask lor 1 in
            {
              acc with
              op_mask = new_mask;
              dmg_mask = acc.dmg_mask lsl 1;
              unkn_mask = acc.unkn_mask lsl 1;
              length = i + 1;
            }
        | '#' ->
            let new_mask = acc.dmg_mask lsl 1 in
            let new_mask = new_mask lor 1 in
            {
              acc with
              dmg_mask = new_mask;
              op_mask = acc.op_mask lsl 1;
              unkn_mask = acc.unkn_mask lsl 1;
              length = i + 1;
            }
        | '?' ->
            let new_mask = acc.unkn_mask lsl 1 in
            let new_mask = new_mask lor 1 in
            {
              acc with
              dmg_mask = acc.dmg_mask lsl 1;
              op_mask = acc.op_mask lsl 1;
              unkn_mask = new_mask;
              length = i + 1;
            }
        | _ -> failwith "only . # and ? supported"
      in
      parse_spring new_acc spring_input (i + 1)
  in
  let parse_line line =
    let split = String.split_on_char ' ' line in
    let spring_input = List.hd split in
    let spring =
      parse_spring { dmg_mask = 0; op_mask = 0; unkn_mask = 0; length = 0; damage_group = [] } spring_input 0
    in
    (* parse the damage group*)
    spring
  in
  let rec aux acc lines =
    match lines with
    | [] -> acc
    | line :: tail ->
        let spring = parse_line line in
        aux (spring :: acc) tail
  in
  List.rev (aux [] input)

let springs = parse_input day12_inputs

let execute () =
  let first_spring = List.hd springs in
  Printf.printf "dmg mask %d and op mask %d" first_spring.dmg_mask first_spring.op_mask;
  Printf.printf "\ndmg mask %s and op mask %s, ? mask %s - length %d"
    (binary_string_of_int first_spring.dmg_mask)
    (binary_string_of_int first_spring.op_mask)
    (binary_string_of_int first_spring.unkn_mask)
    first_spring.length;
  0

let execute' () = 0
