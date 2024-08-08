let day1_inputs = Utils.read_lines "data/advent_1_data.txt"

let () = Printf.printf "\nResult Day1 **: %d\n" (Day1.all_calib_values day1_inputs)

let () = Printf.printf "\nResult Day 2 *: %d\n" Day2.execute_day_2