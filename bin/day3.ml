let day3_inputs = Utils.read_lines "data/advent_3_data.txt"
(* let previous_line = List.nth day3_inputs 0
   let first_line = List.nth day3_inputs 1
   let second_line = List.nth day3_inputs 2 *)

let check_line line prev_line next_line =
  (*Check around and diagonall, line above and under if there is a symbol around index i*)
  let check_symbol_around i =
    let check_is_symbol str i =
      if i < 0 || i >= String.length str then false
      else
        (* let () = Printf.printf "Checking at %d: %c\n" i line.[i] in *)
        match str.[i] with '0' .. '9' | '.' | ' ' -> false | _ -> true
    in

    let check_pos ln =
      List.exists (fun offset -> check_is_symbol ln (i + offset)) [ -1; 0; 1 ]
    in

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
          str_check (i + 1)
            (str_acc ^ String.make 1 c)
            (l + 1) ~valid:(valid || symbol_around)
      | _ -> (result, l)
  in

  let rec aux i acc =
    if i >= String.length line then acc
    else
      match line.[i] with
      | '0' .. '9' ->
          (* if it's a number char, we try to see if there are other numbers after, to return the full*)
          let str_num, l = str_check i "" 0 in
          (* Printf.printf "Str: %s - %d\n" str_num l; *)
          aux (i + l) (acc + int_of_string str_num)
      | '.' -> aux (i + 1) acc
      | _ -> aux (i + 1) acc
  in
  aux 0 0

let check_line' line prev_line next_line =
  let rec get_gear_num ?(start_idx = -1) i str_acc ln =
    let reverse = str_acc != "" && start_idx = -1 in
    let reverse_or_return =
      if reverse then get_gear_num (i + 1) "" ln ~start_idx:(i + 1)
      else (str_acc, start_idx)
    in

    if i < 0 || i >= String.length ln then reverse_or_return
    else
      match ln.[i] with
      | '0' .. '9' as c ->
          (* let () = Printf.printf "i: %d, str_acc: %s\n" i str_acc in *)
          let next_i = if start_idx = -1 then i - 1 else i + 1 in
          let next_str =
            if start_idx = -1 then String.make 1 c
            else str_acc ^ String.make 1 c
          in

          (* let () =
               Printf.printf "Next i: %d, next_str %s, start_idx %d \n" next_i
                 next_str start_idx
             in *)
          let num_start, s_idx = get_gear_num next_i next_str ln ~start_idx in

          (* let () =
               Printf.printf "Num start: %s, start_idx %d \n" num_start start_idx
             in *)

          (* if index -1, we were reversing, we found the start of the string and now we move forward to find the full number *)
          (* if start_idx = -1 then get_gear_num (i + 1) num_start ln ~start_idx:i *)
          (* else num_start *)
          (num_start, s_idx)
      | _ -> reverse_or_return
  in

  let get_gear_ratio i =
    let get_num_at i ln =
      let tl_i = i - 1 in
      let () = Printf.printf "Top left: %d\n" tl_i in
      let tl_num, tl_start = get_gear_num tl_i "" ln in
      let could_have_tm = tl_start = -1 in
      let tm_num, tm_start =
        if could_have_tm then get_gear_num i "" ln else ("", -1)
      in

      (* let () = Printf.printf "Top left: %s\n" tl_num in *)
      (* let () = Printf.printf "Could have top left %b\n" could_have_tm in *)
      (*top right*)
      let could_have_tr =
        tm_start = -1
        && (tl_start = -1 || tl_start + String.length tl_num - tl_i = 1)
      in
      let tr_i = i + 1 in
      let tr_num, tr_start =
        if could_have_tr then get_gear_num tr_i "" ln else ("", -1)
      in

      let () = Printf.printf "could have another number %b\n" could_have_tr in
      let () = Printf.printf "Gear num tl: %s i:%d\n" tl_num tl_start in
      let () = Printf.printf "Gear num tm: %s i:%d\n" tm_num tm_start in
      let () = Printf.printf "Gear num tr: %s i:%d\n" tr_num tr_start in

      List.filter (fun x -> x != "") [ tl_num; tm_num; tr_num ]
    in

    let () = Printf.printf "prev line %s\n" prev_line in
    let () = Printf.printf "line line %s\n" line in
    let () = Printf.printf "next line %s\n" next_line in
    get_num_at i prev_line @ get_num_at i line @ get_num_at i next_line
  in

  let rec aux i acc =
    if i >= String.length line then acc
    else
      match line.[i] with
      | '*' ->
          let gear_values = get_gear_ratio i in
          (*todo check only if values are 2*)
          let gear_ratio =
            if List.length gear_values = 2 then
              List.fold_left (fun acc x -> acc * int_of_string x) 1 gear_values
            else 0
          in
          let () = Printf.printf "!!!Gear ratio: %d\n" gear_ratio in
          aux (i + 1) (acc + gear_ratio)
      | _ -> aux (i + 1) acc
  in
  aux 0 0

let execute =
  let rec fold_lines acc current_line prev_line lines =
    (* let () = Printf.printf "Current line: %s - acc %d\n" current_line acc in *)
    match lines with
    | [] -> acc + check_line current_line prev_line ""
    | next_line :: tl ->
        let new_acc = acc + check_line current_line prev_line next_line in
        fold_lines new_acc next_line current_line tl
  in
  (* start with line one, prev line is empty and all the rest of the lines*)
  fold_lines 0 (List.hd day3_inputs) "" (List.tl day3_inputs)

(* let test =
  let l1 = "....................................18..........889.270.....748.280...997.................617..............622........763............476...." in
  let l2 = "...529......434.....191..489...717...@.....................&....................939*7.....*....................606............760....*......" in
  let l3 = "....*...473....*221................$........182......812........493.84....793..........794.......589..407..41...*.....................68...." in
  let test_val = check_line' l2 l1 l3 in
  Printf.printf "check line: %d\n" test_val;
  0 *)

let execute' =
  let rec fold_lines acc current_line prev_line lines =
    (* let () = Printf.printf "Current line: %s - acc %d\n" current_line acc in *)
    match lines with
    | [] -> acc + check_line' current_line prev_line ""
    | next_line :: tl ->
        let new_acc = acc + check_line' current_line prev_line next_line in
        fold_lines new_acc next_line current_line tl
  in
  (* start with line one, prev line is empty and all the rest of the lines*)
  fold_lines 0 (List.hd day3_inputs) "" (List.tl day3_inputs)