let day13_inputs = Utils.read_lines "data/advent_13_data.txt"

let parse_pattern_horizontally input =
  let rec aux all_lines acc acc_lst =
    match all_lines with
    | [] -> List.rev acc :: acc_lst
    | l :: rest ->
        let is_empty = String.trim l = "" in
        if is_empty then aux rest [] (List.rev acc :: acc_lst) else aux rest (l :: acc) acc_lst
  in
  aux input [] []

let hor_patterns = parse_pattern_horizontally day13_inputs

let change_direction pattern =
  let rec process_line acc line =
    if List.is_empty acc then
      let size = String.length line in
      let new_acc = List.init size (fun _ -> "") in
      process_line new_acc line
    else
      let str_line = String.to_seq line |> Seq.map (String.make 1) |> List.of_seq in
      List.map2 (fun a l -> a ^ l) acc str_line
  in
  let rec aux acc p =
    match p with
    | [] -> acc
    | l :: rest ->
        let new_acc = process_line acc l in
        aux new_acc rest
  in
  aux [] pattern

let change_direction_all patterns =
  let rec aux acc pattern =
    match pattern with
    | [] -> acc
    | p :: rest ->
        let new_dir = change_direction p in
        aux (new_dir :: acc) rest
  in
  aux [] patterns

let vrt_patterns = change_direction_all hor_patterns

let check_reflection ?(exclude = -1) (pattern : string array) =
  let rec aux p i l1 l2 =
    if i + 1 >= Array.length p then 0
    else if i + 1 = exclude then aux p (i + 1) (i + 1) (i + 2)
    else if l1 < 0 || l2 >= Array.length p then i + 1
    else if p.(l1) = p.(l2) then aux p i (l1 - 1) (l2 + 1)
    else aux p (i + 1) (i + 1) (i + 2)
  in
  aux pattern 0 0 1

let check_reflection_smudge pattern =
  let smudge_string i str =
    let smudge_char c i = String.sub str 0 i ^ c ^ String.sub str (i + 1) (String.length str - (i + 1)) in
    match str.[i] with
    | '.' -> smudge_char "#" i
    | '#' -> smudge_char "." i
    | _ -> failwith "only . and # are valid chars"
  in
  let smudge pattern char_i line =
    let new_line = pattern.(line) |> smudge_string char_i in
    let new_arr = Array.copy pattern in
    new_arr.(line) <- new_line;
    new_arr
  in
  let rec aux pattern char_i line prev_reflection =
    if line >= Array.length pattern then 0
    else if char_i >= String.length pattern.(line) then aux pattern 0 (line + 1) prev_reflection
    else
      let result = smudge pattern char_i line |> check_reflection ~exclude:prev_reflection in
      if result <> 0 && result <> prev_reflection then result else aux pattern (char_i + 1) line prev_reflection
  in
  let prev_reflection = check_reflection pattern in
  aux pattern 0 0 prev_reflection

let reflect_with reflection_fun =
  List.fold_left2
    (fun acc v h -> acc + reflection_fun (Array.of_list v) + (100 * reflection_fun (Array.of_list h)))
    0 vrt_patterns hor_patterns

let execute () = reflect_with check_reflection
let execute' () = reflect_with check_reflection_smudge
