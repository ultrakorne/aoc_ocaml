open Stdint

let day12_inputs = Utils.read_lines "data/advent_12_data.txt"

type spring = { dmg_mask : int128; op_mask : int128; unkn_mask : int128; length : int }
type spring_line = { spring : spring; damage_group : int list }

let binary_string_of_int n =
  let rec aux n acc = if n = 0 then acc else aux (n lsr 1) (string_of_int (n land 1) ^ acc) in
  if n = 0 then "0" else aux n ""

let set_bits n =
  let rec aux acc n' =
    if n' <= 0 then acc
    else
      let acc = Int128.shift_left acc 1 in
      let acc = Int128.logor acc Int128.one in
      aux acc (n' - 1)
  in
  aux Int128.zero n

let parse_input input =
  let rec parse_spring acc spring_input i =
    if i >= String.length spring_input then acc
    else
      let new_mask =
        {
          dmg_mask = Int128.shift_left acc.dmg_mask 1;
          op_mask = Int128.shift_left acc.op_mask 1;
          unkn_mask = Int128.shift_left acc.unkn_mask 1;
          length = acc.length + 1;
        }
      in
      let new_acc =
        match spring_input.[i] with
        | '.' -> { new_mask with op_mask = Int128.logor new_mask.op_mask Int128.one }
        | '#' -> { new_mask with dmg_mask = Int128.logor new_mask.dmg_mask Int128.one  }
        | '?' -> { new_mask with unkn_mask = Int128.logor new_mask.unkn_mask Int128.one }
        | _ -> failwith "only . # and ? supported"
      in
      parse_spring new_acc spring_input (i + 1)
  in
  let parse_line line =
    let split = String.split_on_char ' ' line in
    let spring_input = List.hd split in
    let nums = List.tl split |> List.hd |> String.split_on_char ',' |> List.map int_of_string in
    let spring_line = parse_spring { dmg_mask = Int128.zero; op_mask = Int128.zero; unkn_mask = Int128.zero; length = 0 } spring_input 0 in
    { spring = spring_line; damage_group = nums }
  in
  let rec aux acc lines =
    match lines with
    | [] -> acc
    | line :: tail ->
        let spring = parse_line line in
        aux (spring :: acc) tail
  in
  List.rev (aux [] input)

let get_subspring spring n =
  let mask = set_bits n in
  {
    dmg_mask = Int128.logand spring.dmg_mask mask;
    op_mask = Int128.logand spring.op_mask mask;
    unkn_mask = Int128.logand spring.unkn_mask mask;
    length = n;
  }

let check_combinations spring_line =
  let rec find_valid_combinations acc comb n shift spring =
    if shift + n > spring.length then acc
    else
      let comb' = Int128.shift_left comb shift in
      let dmg_masked = Int128.logor comb' spring.dmg_mask in
      (* checking the left part of the mask, if its the same as comb. -1 is used because we still want to make sure there is a space on the right *)
      let check_comb = Int128.shift_right_logical comb' (Int.max 0 (shift - 1)) in
      let dmg_masked_check = Int128.shift_right_logical dmg_masked (Int.max 0 (shift - 1)) in

      (* Printf.printf "\ndmg_masked %s " (binary_string_of_int dmg_masked);
         Printf.printf "check_comb %s " (binary_string_of_int check_comb );
         Printf.printf "dmg_masked_check  %s " (binary_string_of_int dmg_masked_check  ); *)
      let is_valid_comb_dmg = check_comb = dmg_masked_check in

      (*check that is not on the ., the op mask since those cannot have damage*)
      let op_masked = Int128.logand comb' spring.op_mask in
      let is_valid_comb_op = op_masked = Int128.zero in
      (* Printf.printf "\n(%d/%d) comb %s is valid for dmg %b and valid op %b" shift spring.length
         (binary_string_of_int comb') is_valid_comb_dmg is_valid_comb_op; *)
      if is_valid_comb_dmg && is_valid_comb_op then
        find_valid_combinations ((comb', shift - 1) :: acc) comb n (shift + 1) spring
      else find_valid_combinations acc comb n (shift + 1) spring
  in

  let rec aux nums spring =
    let rec traverse_comb acc combs rest_nums spring =
      match combs with
      | [] -> acc
      | c :: tail ->
          (* Printf.printf "\nTraversing combination %s" (binary_string_of_int (fst c)); *)
          let sub_spring = get_subspring spring (snd c) in
          let valid = aux rest_nums sub_spring in

          (* Printf.printf "\ncombination %s is valid %d " (binary_string_of_int (fst c)) valid;
             Printf.printf "\n dmg mask %s -> rest nums %d"
               (binary_string_of_int sub_spring.dmg_mask)
               (List.length rest_nums); *)

          (*edge case .##.?#??.#.?# 2,1,1,1 if this is the last number to check and the dmg_mask is not 0 it means there is still a symbol # on the right side that has not been fulfilled so this solution should be discarded*)
          let valid = if List.length rest_nums = 0 && sub_spring.dmg_mask <> Int128.zero then 0 else valid in
          traverse_comb (acc + valid) tail rest_nums spring
    in

    (* TODO for every combs , we expand based on the remaining length*)
    match nums with
    | [] -> 1
    | n :: rest ->
        let start = set_bits n in
        (* Printf.printf "\n start find valid comb for num %d: %s" n (binary_string_of_int start); *)
        let combs = find_valid_combinations [] start n 0 spring in
        (* List.iter (fun c -> Printf.printf "\n valid combinations %s %d" (binary_string_of_int (fst c)) (snd c)) combs; *)
        if List.is_empty combs then 0
        else
          (* combs snd has the lenght of how much left there is for the rest of the nums *)
          traverse_comb 0 combs rest spring
  in

  aux spring_line.damage_group spring_line.spring

let springs = parse_input day12_inputs

let execute () =
  let first_spring = List.hd springs in
  (* Printf.printf "dmg mask %d and op mask %d, ?mask = %d" first_spring.spring.dmg_mask first_spring.spring.op_mask
    first_spring.spring.unkn_mask; *)
  Printf.printf "\ndmg mask %s and op mask %s, ? mask %s - length %d"
    (Int128.to_string_bin first_spring.spring.dmg_mask)
    (Int128.to_string_bin  first_spring.spring.op_mask)
    (Int128.to_string_bin  first_spring.spring.unkn_mask)
    first_spring.spring.length;
  List.fold_left (fun acc x -> acc + check_combinations x) 0 springs

let execute' () = 0
