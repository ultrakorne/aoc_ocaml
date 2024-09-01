let day12_inputs = Utils.read_lines "data/advent_12_data.txt"

type spring = { dmg_mask : int; op_mask : int; unkn_mask : int; length : int; damage_group : int list }

let binary_string_of_int n =
  let rec aux n acc = if n = 0 then acc else aux (n lsr 1) (string_of_int (n land 1) ^ acc) in
  if n = 0 then "0" else aux n ""

let set_bits n =
  let rec aux acc n' =
    if n' <= 0 then acc
    else
      let acc = acc lsl 1 in
      let acc = acc lor 1 in
      aux acc (n' - 1)
  in
  aux 0 n

let parse_input input =
  let rec parse_spring acc spring_input i =
    if i >= String.length spring_input then acc
    else
      let new_mask =
        {
          acc with
          dmg_mask = acc.dmg_mask lsl 1;
          op_mask = acc.op_mask lsl 1;
          unkn_mask = acc.unkn_mask lsl 1;
          length = acc.length + 1;
        }
      in
      let new_acc =
        match spring_input.[i] with
        | '.' -> { new_mask with op_mask = new_mask.op_mask lor 1 }
        | '#' -> { new_mask with dmg_mask = new_mask.dmg_mask lor 1 }
        | '?' -> { new_mask with unkn_mask = new_mask.unkn_mask lor 1 }
        | _ -> failwith "only . # and ? supported"
      in
      parse_spring new_acc spring_input (i + 1)
  in
  let parse_line line =
    let split = String.split_on_char ' ' line in
    let spring_input = List.hd split in
    let nums = List.tl split |> List.hd |> String.split_on_char ',' |> List.map int_of_string in
    parse_spring { dmg_mask = 0; op_mask = 0; unkn_mask = 0; length = 0; damage_group = nums } spring_input 0
  in
  let rec aux acc lines =
    match lines with
    | [] -> acc
    | line :: tail ->
        let spring = parse_line line in
        aux (spring :: acc) tail
  in
  List.rev (aux [] input)

let check_combinations spring =
  let rec find_valid_combinations acc comb n shift shift_max =
    if shift + n > shift_max then acc
    else
      let comb' = comb lsl shift in
      let dmg_masked = comb' lor spring.dmg_mask in
      (* checking the left part of the mask, if its the same as comb. -1 is used because we still want to make sure there is a space on the right *)
        let check_comb = comb' lsr (Int.max 0 (shift -1)) in
      let dmg_masked_check = dmg_masked lsr (Int.max 0 (shift -1)) in
      
      (* Printf.printf "\ndmg_masked %s " (binary_string_of_int dmg_masked);
      Printf.printf "check_comb %s " (binary_string_of_int check_comb );
      Printf.printf "dmg_masked_check  %s " (binary_string_of_int dmg_masked_check  ); *)
      
      let is_valid_comb_dmg = check_comb = dmg_masked_check in

      (*check that is not on the ., the op mask since those cannot have damage*)
      let op_masked = comb' land spring.op_mask in
      let is_valid_comb_op = op_masked = 0 in
      Printf.printf "\n(%d/%d) comb %s is valid for dmg %b and valid op %b" 
     shift shift_max
      (binary_string_of_int comb') is_valid_comb_dmg is_valid_comb_op;
      if is_valid_comb_dmg && is_valid_comb_op then 
        find_valid_combinations ((comb', shift - 1)::acc) comb n (shift + 1) shift_max 
      else
        find_valid_combinations acc comb n (shift + 1) shift_max 

  in

  let rec aux nums length =
    (* TODO for every combs , we expand based on the remaining length*)
    match nums with
    | [] -> 0
    | n :: rest ->
        let start = set_bits n in
        Printf.printf "\n start find valid comb for n %d: %s" n (binary_string_of_int start);
        let combs = find_valid_combinations [] start n 0 length in
        List.iter (fun c -> Printf.printf "\n valid comb %s %d" (binary_string_of_int (fst c)) (snd c)) combs;
        (* new lenght TODO*)
        (*
        *)
        aux rest length
  in
  aux spring.damage_group spring.length

let springs = parse_input day12_inputs

let execute () =
  let first_spring = List.hd springs in
  Printf.printf "dmg mask %d and op mask %d" first_spring.dmg_mask first_spring.op_mask;
  Printf.printf "\ndmg mask %s and op mask %s, ? mask %s - length %d"
    (binary_string_of_int first_spring.dmg_mask)
    (binary_string_of_int first_spring.op_mask)
    (binary_string_of_int first_spring.unkn_mask)
    first_spring.length;
  check_combinations first_spring

let execute' () = 0
