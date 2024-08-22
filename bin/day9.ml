let day9_inputs = Utils.read_lines "data/advent_9_data.txt"

let test_line = List.nth day9_inputs 0

let parse_line line = String.split_on_char ' ' line |> List.map int_of_string

let test_result = parse_line test_line
let () = List.iter (Printf.printf "\n %d ") test_result

let execute () = 0
let execute' () = 0
