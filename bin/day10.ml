let day10_inputs = Utils.read_lines "data/advent_10_data.txt"

(* y is row, x is column*)
type coord = { x : int; y : int }

module Matrix = Map.Make (struct
  type t = coord

  let compare = compare
end)

let connection_of_symbol symbol sym_c c =
  match symbol with
  | 'J' -> (c.x + 1 = sym_c.x && c.y = sym_c.y) || (c.y + 1 = sym_c.y && c.x = sym_c.x)
  | 'L' -> (c.x - 1 = sym_c.x && c.y = sym_c.y) || (c.y + 1 = sym_c.y && c.x = sym_c.x)
  | 'F' -> (c.x - 1 = sym_c.x && c.y = sym_c.y) || (c.y - 1 = sym_c.y && c.x = sym_c.x)
  | '7' -> (c.x + 1 = sym_c.x && c.y = sym_c.y) || (c.y - 1 = sym_c.y && c.x = sym_c.x)
  | 'S' -> Int.abs (c.x - sym_c.x) <= 1 && Int.abs (c.y - sym_c.y) <= 1
  | '|' -> c.x = sym_c.x && Int.abs (c.y - sym_c.y) = 1
  | '-' -> c.y = sym_c.y && Int.abs (c.x - sym_c.x) = 1
  | _ -> false

let adjacent matrix coord =
  let sum_coord c1 c2 = { x = c1.x + c2.x; y = c1.y + c2.y } in
  let around = [ { x = 0; y = -1 }; { x = 1; y = 0 }; { x = 0; y = 1 }; { x = -1; y = 0 } ] in
  List.map
    (fun x ->
      let new_coord = sum_coord coord x in
      (new_coord, Matrix.find_opt new_coord matrix))
    around
  |> List.filter (fun x -> snd x <> None)
  |> List.map (function c, Some s -> (c, s) | _, None -> failwith "never")

let adjacent_connected matrix element =
  let connected (c1, s1) (c2, s2) = connection_of_symbol s1 c1 c2 && connection_of_symbol s2 c2 c1 in
  let adj = fst element |> adjacent matrix in
  List.filter (fun (c, s) -> connected element (c, s)) adj

(* return a list of (coord, char) as fst and snd the coord of the starting position 'S'*)
let parse_input input =
  let rec parse_row seq x y mtx_lst start =
    match seq () with
    | Seq.Nil -> (mtx_lst, start)
    | Seq.Cons (s, tail) ->
        let start_coord = if start <> None then start else if s = 'S' then Some { x; y } else None in
        let new_lst = ({ x; y }, s) :: mtx_lst in
        parse_row tail (x + 1) y new_lst start_coord
  in
  let rec aux line y mtx_lst start =
    match line with
    | [] -> (mtx_lst, start)
    | l :: tail ->
        (* let () = Printf.printf "\nsequencing line %d %!" y in *)
        let seq = String.to_seq l in
        (* let () = Printf.printf "\nparsing line %d %!" y in *)
        let row, start_coord = parse_row seq 0 y [] start in
        (* let () = Printf.printf "\nrecurs call line %d list has %d elements %!" y (List.length mtx_lst) in *)
        aux tail (y + 1) (List.rev_append row mtx_lst) start_coord
  in
  aux input 0 [] None

let matrix_list, start = parse_input day10_inputs
let matrix = Matrix.of_list matrix_list

let get_loop matrix start =
  let rec aux symbol loop =
    let adj = adjacent_connected matrix symbol in
    (* keep only adj elements that are not part of the loop yet*)
    let new_loop_elements =
      List.filter
        (fun (x, _) ->
          let el = Matrix.find_opt x loop in
          match el with Some _ -> false | None -> true)
        adj
    in
    (* if all adj elements are part of the loop, we are back at start and can return the loop*)
    if List.length new_loop_elements = 0 then loop
    else
      (* even if we have 2 connected elemets, we start looping in the first direction*)
      let next_elem = List.hd new_loop_elements in
      (* let () = Printf.printf " > next %c %!" (snd next_elem) in *)
      let new_loop = Matrix.add (fst next_elem) (snd next_elem) loop in
      aux next_elem new_loop
  in
  Matrix.of_list [ start ] |> aux start

let execute () =
  (* let coord1 = Matrix.find { x = 2; y = 1 } matrix in *)
  match start with
  | Some start_coord ->
      (* let adj = adjacent matrix start_coord in *)
      (* let () = Printf.printf "\nstart x:%d y:%d" start_coord.x start_coord.y in
         let () = Printf.printf "\nvalue %c" coord1 in
         let () = List.iter (fun x -> Printf.printf "\n adj to start: %c at %d,%d " (snd x) (fst x).x (fst x).y) adj in *)
      (* let adj_conn = adjacent_connected matrix (start_coord, 'S') in *)

      (* let () =
           List.iter (fun x -> Printf.printf "\n adj conn to start: %c at %d,%d " (snd x) (fst x).x (fst x).y) adj_conn
         in *)
      let loop = get_loop matrix (start_coord, 'S') in
      let loop_size = Matrix.to_list loop |> List.length in
      let () = Printf.printf "\nloop size %d " loop_size in
      loop_size / 2
  | None ->
      let () = Printf.printf "no start character S found" in
      0

let execute' () = 0
