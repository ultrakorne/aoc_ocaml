open Stdint

let day12_inputs = Utils.read_lines "data/advent_12_data.txt"

type spring = { dmg_mask : int128; op_mask : int128; unkn_mask : int128; length : int }
type spring_line = { spring : spring; damage_group : int list }

module StringMap = Map.Make (String)

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
        | '#' -> { new_mask with dmg_mask = Int128.logor new_mask.dmg_mask Int128.one }
        | '?' -> { new_mask with unkn_mask = Int128.logor new_mask.unkn_mask Int128.one }
        | _ -> failwith "only . # and ? supported"
      in
      parse_spring new_acc spring_input (i + 1)
  in
  let parse_line line =
    let split = String.split_on_char ' ' line in
    let spring_input = List.hd split in
    let nums = List.tl split |> List.hd |> String.split_on_char ',' |> List.map int_of_string in
    let spring_line =
      parse_spring { dmg_mask = Int128.zero; op_mask = Int128.zero; unkn_mask = Int128.zero; length = 0 } spring_input 0
    in
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

type combination = { bitmap : Int128.t; shift : int; num_bits : int }

let key_from_combinations combs nums spring =
  let key =
    List.fold_left (fun acc comb -> acc ^ string_of_int comb.num_bits ^ "<" ^ string_of_int comb.shift ^ ",") "" combs
  in
  (List.fold_left (fun acc num -> acc ^ "|" ^ string_of_int num) key nums) ^ ":" ^ string_of_int spring.length

let check_combinations spring_line =
  (*chaching in this map the result of trees of results*)
  let key_map = ref StringMap.empty in
  let rec find_valid_combinations acc comb n shift spring =
    if shift + n > spring.length then acc
    else
      let comb' = Int128.shift_left comb shift in
      let dmg_masked = Int128.logor comb' spring.dmg_mask in
      (* checking the left part of the mask, if its the same as comb. -1 is used because we still want to make sure there is a space on the right *)
      let check_comb = Int128.shift_right_logical comb' (Int.max 0 (shift - 1)) in
      let dmg_masked_check = Int128.shift_right_logical dmg_masked (Int.max 0 (shift - 1)) in
      let is_valid_comb_dmg = check_comb = dmg_masked_check in

      (*check that is not on the ., the op mask since those cannot have damage*)
      let op_masked = Int128.logand comb' spring.op_mask in
      let is_valid_comb_op = op_masked = Int128.zero in
      if is_valid_comb_dmg && is_valid_comb_op then
        find_valid_combinations ({ bitmap = comb'; shift = shift - 1; num_bits = n } :: acc) comb n (shift + 1) spring
      else find_valid_combinations acc comb n (shift + 1) spring
  in

  let rec aux nums spring =
    let rec traverse_comb acc combs rest_nums spring =
      match combs with
      | [] -> acc
      | c :: tail ->
          let sub_spring = get_subspring spring c.shift in
          let valid = aux rest_nums sub_spring in

          (*edge case .##.?#??.#.?# 2,1,1,1 if this is the last number to check and the dmg_mask is not 0 it means there is still a symbol # on the right side that has not been fulfilled so this solution should be discarded*)
          let valid = if List.length rest_nums = 0 && sub_spring.dmg_mask <> Int128.zero then 0 else valid in
          let result = traverse_comb (acc + valid) tail rest_nums spring in

          result
    in

    match nums with
    | [] -> 1
    | n :: rest -> (
        let start = set_bits n in
        let combs = find_valid_combinations [] start n 0 spring in
        if List.is_empty combs then 0
        else
          let key = key_from_combinations combs rest spring in
          match StringMap.find_opt key !key_map with
          | None ->
              let result = traverse_comb 0 combs rest spring in
              key_map := StringMap.add key result !key_map;
              result
          | Some v -> v)
  in

  aux spring_line.damage_group spring_line.spring

let springs = parse_input day12_inputs

let make_longer springs =
  let rec aux acc springs =
    let rec extend_spring qs s i =
      if i = 0 then qs
      else
        let new_dmg_mask = Int128.shift_left qs.spring.dmg_mask (s.spring.length + 1) in
        let new_dmg_mask = Int128.logor new_dmg_mask s.spring.dmg_mask in

        let new_op_mask = Int128.shift_left qs.spring.op_mask (s.spring.length + 1) in
        let new_op_mask = Int128.logor new_op_mask s.spring.op_mask in

        let new_unkn_mask = Int128.shift_left qs.spring.unkn_mask 1 |> Int128.logor Int128.one in
        let new_unkn_mask = Int128.shift_left new_unkn_mask s.spring.length in
        let new_unkn_mask = Int128.logor new_unkn_mask s.spring.unkn_mask in

        let new_lenght = qs.spring.length + s.spring.length + 1 in
        let next_s =
          { dmg_mask = new_dmg_mask; op_mask = new_op_mask; unkn_mask = new_unkn_mask; length = new_lenght }
        in
        let new_qs = { spring = next_s; damage_group = s.damage_group @ qs.damage_group } in
        extend_spring new_qs s (i - 1)
    in

    match springs with
    | [] -> acc
    | s :: rest ->
        let longer = extend_spring s s 4 in
        aux (longer :: acc) rest
  in
  aux [] springs

let springs' = make_longer springs

let process_springs springs =
  List.fold_left
    (fun acc x ->
      let combs = check_combinations x in
      acc + combs)
    0 springs

let execute () =
  let t = Sys.time () in
  let result = process_springs springs in
  Printf.printf "\nExecution time: %fs\n" (Sys.time () -. t);
  result

let execute' () =
  let t = Sys.time () in
  let result = process_springs springs' in
  Printf.printf "\nExecution time: %fs\n" (Sys.time () -. t);
  result
