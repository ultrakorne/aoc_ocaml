let day3_inputs = Utils.read_lines "data/advent_3_data.txt"
let previous_line = List.nth day3_inputs 0
let first_line = List.nth day3_inputs 1
let second_line = List.nth day3_inputs 2

(*Check if there is a symbol around the index i*)

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

    let symbol_found =
      check_pos prev_line || check_pos line || check_pos next_line
    in

    (* if symbol_found then
      Printf.printf "Symbol found at %d around %c\n" i line.[i]; *)
    symbol_found
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

(* let execute =
  let test_val = check_line first_line previous_line second_line in
  Printf.printf "First line: %d\n" test_val;
  0 *)
