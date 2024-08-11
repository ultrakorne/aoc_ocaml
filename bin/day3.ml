let day3_inputs = Utils.read_lines "data/advent_3_data.txt"
let first_line = List.nth day3_inputs 0
let second_line = List.nth day3_inputs 1

let check_line line prev_line next_line =
  (*Check around and diagonall, line above and under if there is a symbol around index i*)
  let check_symbol_around i =
    let check_is_symbol str i =
      if i < 0 || i >= String.length str then false
      else 
        let () = Printf.printf "Checking at %d: %c\n" i line.[i] in
        match str.[i] with '0' .. '9' | '.' | ' ' -> false | _ -> true
    in
    let tc = check_is_symbol prev_line i in
    let bc = check_is_symbol next_line i in
    let symbol_found = tc || bc in
    if symbol_found then
      Printf.printf "Symbol found at %d around %c\n" i line.[i];
    symbol_found
  in

  (*return a tuple with the string number and the length of the string number if the number is invalid returns 0*)
  let rec str_check ?(valid = false) i str_acc l =
    if i >= String.length line then ((if valid then str_acc else "0"), l)
    else
      match line.[i] with
      | '0' .. '9' as c ->
          let symbol_around = check_symbol_around i in
          str_check (i + 1)
            (str_acc ^ String.make 1 c)
            (l + 1) ~valid:(valid || symbol_around)
      | _ -> ((if valid then str_acc else "0"), l)
  in

  let rec aux i acc =
    if i >= String.length line then acc
    else
      (* let () = Printf.printf "Char: %c\n" line.[i] in *)
      match line.[i] with
      | '0' .. '9' as c ->
          (* if it's a number char, we try to see if there are other numbers after, to return the full*)
          let str_num, l = str_check (i + 1) (String.make 1 c) 1 in
          Printf.printf "Str: %s - %d\n" str_num l;
          aux (i + l) (acc + int_of_string str_num)
      | '.' -> aux (i + 1) acc
      | _ -> aux (i + 1) acc
  in
  aux 0 0

let execute =
  let test_val = check_line first_line "" second_line in
  Printf.printf "First line: %d\n" test_val;
  0
