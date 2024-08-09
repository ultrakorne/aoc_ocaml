let numbers_spelled =
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let starts_with lst str =
  List.find_index (fun prefix -> String.starts_with ~prefix str) lst

let substring_from_index str i = String.sub str i (String.length str - i)

(*Second star day 1 2023*)
let calib_value' v =
  let rec aux first last i =
    if i >= String.length v then
      int_of_string (String.make 1 first ^ String.make 1 last)
    else
      match v.[i] with
      | '0' .. '9' as c ->
          if first = ' ' then aux c c (i + 1) else aux first c (i + 1)
      | _ -> (
          match starts_with numbers_spelled (substring_from_index v i) with
          | Some idx ->
              let num = idx + 1 in
              let c = char_of_int (num + 48) in
              if first = ' ' then aux c c (i + 1) else aux first c (i + 1)
          | None -> aux first last (i + 1))
  in
  aux ' ' ' ' 0

(*First star day 1 2023*)
let calib_value v =
  let rec aux first last i =
    if i >= String.length v then
      int_of_string (String.make 1 first ^ String.make 1 last)
    else
      match v.[i] with
      | '0' .. '9' as c ->
          if first = ' ' then aux c c (i + 1) else aux first c (i + 1)
      | _ -> aux first last (i + 1)
  in
  aux ' ' ' ' 0

let day1_inputs = Utils.read_lines "data/advent_1_data.txt"
let execute_day_1 = List.fold_left (fun acc v -> acc + calib_value v) 0 day1_inputs
let execute_day_1' = List.fold_left (fun acc v -> acc + calib_value' v) 0 day1_inputs