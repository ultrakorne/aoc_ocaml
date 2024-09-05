let day13_inputs = Utils.read_lines "data/advent_13_data_test.txt"

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

let check_reflection (pattern : string array) =
  let rec aux p i l1 l2 =
    if i + 1 >= Array.length p then 0
    else if l1 < 0 || l2 >= Array.length p then i + 1
    else if p.(l1) = p.(l2) then aux p i (l1 - 1) (l2 + 1)
    else aux p (i + 1) (i + 1) (i + 2)
  in
  Array.iter (fun s -> Printf.printf "\n%s" s) pattern;
  let result = aux pattern 0 0 1 in
  Printf.printf "\nresult %d" result;
  result

let check_reflection_smudge pattern =
  let smudge_string i str =
    match str.[i] with
    | '.' -> String.sub str 0 (i - 1) ^ "#" ^ String.sub str (i + 1) (String.length str - (i + 1))
    | '#' -> String.sub str 0 (i - 1) ^ "." ^ String.sub str (i + 1) (String.length str - (i + 1))
    | _ -> failwith "only . and # are valid chars"
  in
  let rec smudge pattern char_i line =
    if line >= Array.length pattern then None
    else
      let pattern_line = pattern.(line) in
      if char_i >= String.length pattern_line then smudge pattern 0 (line + 1)
      else
        let new_line = smudge_string char_i pattern_line in
        let new_arr = Array.copy pattern in
        new_arr.(line) <- new_line;
        Some new_arr
  in
  let rec aux pattern char_i line =
    let smudged_pattern = smudge pattern char_i line in
    match smudged_pattern with
    | None -> 0
    | Some smudged ->
        let result = check_reflection smudged in
        if result <> 0 then result else aux pattern (char_i + 1) line
  in
  aux pattern 0 0

let execute () =
  Printf.printf "\nvertical patterns to check %d" (List.length vrt_patterns);
  List.fold_left (fun acc x -> acc + check_reflection (Array.of_list x)) 0 vrt_patterns
  + (100 * List.fold_left (fun acc x -> acc + check_reflection (Array.of_list x)) 0 hor_patterns)

let execute' () = 0
