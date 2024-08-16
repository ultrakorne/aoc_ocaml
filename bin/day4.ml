let day4_inputs = Utils.read_lines "data/advent_4_data_test.txt"

let process_card card_input =
  let rm_empty str_list = List.filter (fun x -> x <> "" && x <> " ") str_list in
  let split = String.split_on_char ':' card_input |> List.rev |> List.hd |> String.split_on_char '|' in
  let winning_nums = List.hd split |> String.split_on_char ' ' |> rm_empty in
  let my_nums = List.rev split |> List.hd |> String.split_on_char ' ' |> rm_empty in
  List.filter (fun x -> List.mem x my_nums) winning_nums |> List.length

let execute =
  let double_points num = if num = 0 then 0 else Base.Int.pow 2 (num - 1) in
  List.fold_left (fun acc s -> acc + (process_card s |> double_points)) 0 day4_inputs

let execute' =
  let arr_inputs = Array.of_list day4_inputs in
  let rec fold_cards acc i =
    if i >= Array.length arr_inputs then acc
    else
      let win_n = arr_inputs.(i) |> process_card in
      let a = Array.init win_n (fun x -> x + 1) in
      let num_cards = Array.fold_left (fun acc' x -> acc' + fold_cards 0 (i + x)) 0 a in
      acc + num_cards + 1
  in
  let arr_indexes = Array.init (Array.length arr_inputs) (fun x -> x) in
  Array.fold_left (fun acc i -> acc + fold_cards 0 i) 0 arr_indexes