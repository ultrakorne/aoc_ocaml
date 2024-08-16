let day3_inputs = Utils.read_lines "data/advent_3_data.txt"

(* first star *)
let check_line line prev_line next_line =
  (*Check around and diagonall, line above and under if there is a symbol around index i*)
  let check_symbol_around i =
    let check_is_symbol str i =
      if i < 0 || i >= String.length str then false
      else match str.[i] with '0' .. '9' | '.' | ' ' -> false | _ -> true
    in

    let check_pos ln = List.exists (fun offset -> check_is_symbol ln (i + offset)) [ -1; 0; 1 ] in
    check_pos prev_line || check_pos line || check_pos next_line
  in

  (*return a tuple with the string number and the length of the string number if the number is invalid returns 0*)
  let rec str_check ?(valid = false) i str_acc l =
    let result = if valid then str_acc else "0" in
    if i >= String.length line then (result, l)
    else
      match line.[i] with
      | '0' .. '9' as c ->
          let symbol_around = check_symbol_around i in
          str_check (i + 1) (str_acc ^ String.make 1 c) (l + 1) ~valid:(valid || symbol_around)
      | _ -> (result, l)
  in

  let rec aux i acc =
    if i >= String.length line then acc
    else
      match line.[i] with
      | '0' .. '9' ->
          (* if it's a number char, we try to see if there are other numbers after, to return the full*)
          let str_num, l = str_check i "" 0 in
          aux (i + l) (acc + int_of_string str_num)
      | '.' -> aux (i + 1) acc
      | _ -> aux (i + 1) acc
  in
  aux 0 0

(* second star *)
let check_line' line prev_line next_line =
  let rec get_gear_num ?(start_idx = -1) i str_acc ln =
    let reverse = str_acc != "" && start_idx = -1 in
    let reverse_or_return = if reverse then get_gear_num (i + 1) "" ln ~start_idx:(i + 1) else (str_acc, start_idx) in

    if i < 0 || i >= String.length ln then reverse_or_return
    else
      match ln.[i] with
      | '0' .. '9' as c ->
          let next_i = if start_idx = -1 then i - 1 else i + 1 in
          let next_str = if start_idx = -1 then String.make 1 c else str_acc ^ String.make 1 c in
          get_gear_num next_i next_str ln ~start_idx
      | _ -> reverse_or_return
  in

  (* returns the array of numbers around i, being the index of the '*' *)
  let get_gear_ratio i =
    let get_num_at i ln =
      let l_i = i - 1 in
      let l_num, l_start = get_gear_num l_i "" ln in
      let m_num, m_start = if l_start = -1 then get_gear_num i "" ln else ("", -1) in

      let could_have_r = m_start = -1 && (l_start = -1 || l_start + String.length l_num - l_i = 1) in

      let r_num, _ = if could_have_r then get_gear_num (i + 1) "" ln else ("", -1) in

      List.filter (fun x -> x != "") [ l_num; m_num; r_num ]
    in

    get_num_at i prev_line @ get_num_at i line @ get_num_at i next_line
  in

  let rec aux i acc =
    if i >= String.length line then acc
    else
      match line.[i] with
      | '*' ->
          let gear_values = get_gear_ratio i in
          let gear_ratio =
            if List.length gear_values = 2 then List.fold_left (fun acc x -> acc * int_of_string x) 1 gear_values else 0
          in
          aux (i + 1) (acc + gear_ratio)
      | _ -> aux (i + 1) acc
  in
  aux 0 0

let rec fold_lines check_fun acc current_line prev_line lines =
  match lines with
  | [] -> acc + check_fun current_line prev_line ""
  | next_line :: tl ->
      let new_acc = acc + check_fun current_line prev_line next_line in
      fold_lines check_fun new_acc next_line current_line tl

let execute = fold_lines check_line 0 (List.hd day3_inputs) "" (List.tl day3_inputs)
let execute' = fold_lines check_line' 0 (List.hd day3_inputs) "" (List.tl day3_inputs)
