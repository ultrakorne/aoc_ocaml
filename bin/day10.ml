let day10_inputs = Utils.read_lines "data/advent_10_data_test_2.txt"

(* y is row, x is column*)
type coord = { x : int; y : int }

module Matrix = Map.Make (struct
  type t = coord

  let compare = compare
end)

let connection_of_symbol ?(res = 1) symbol sym_c c =
  match symbol with
  | 'J' -> (c.x + res = sym_c.x && c.y = sym_c.y) || (c.y + 1 = sym_c.y && c.x = sym_c.x)
  | 'L' -> (c.x - res = sym_c.x && c.y = sym_c.y) || (c.y + 1 = sym_c.y && c.x = sym_c.x)
  | 'F' -> (c.x - res = sym_c.x && c.y = sym_c.y) || (c.y - 1 = sym_c.y && c.x = sym_c.x)
  | '7' -> (c.x + res = sym_c.x && c.y = sym_c.y) || (c.y - 1 = sym_c.y && c.x = sym_c.x)
  | 'S' -> Int.abs (c.x - sym_c.x) <= res && Int.abs (c.y - sym_c.y) <= res
  | '|' -> c.x = sym_c.x && Int.abs (c.y - sym_c.y) = res
  | '-' -> c.y = sym_c.y && Int.abs (c.x - sym_c.x) = res
  | _ -> false

let sum_coord c1 c2 = { x = c1.x + c2.x; y = c1.y + c2.y }

let adjacent matrix coord =
  let around = [ { x = 0; y = -1 }; { x = 1; y = 0 }; { x = 0; y = 1 }; { x = -1; y = 0 } ] in
  List.map
    (fun x ->
      let new_coord = sum_coord coord x in
      (new_coord, Matrix.find_opt new_coord matrix))
    around
  |> List.filter (fun x -> snd x <> None)
  |> List.map (function c, Some s -> (c, s) | _, None -> failwith "never")

let connected ?(res = 1) (c1, s1) (c2, s2) = connection_of_symbol s1 c1 c2 ~res && connection_of_symbol s2 c2 c1 ~res

let adjacent_connected ?(res = 1) matrix element =
  let adj = fst element |> adjacent matrix in
  List.filter (fun (c, s) -> connected ~res element (c, s)) adj

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

let inc_resolution matrix_lst =
  let rec aux mtx acc =
    match mtx with
    | [] -> acc
    | (coord, c) :: tail ->
        let m_coord = { x = coord.x * 2; y = coord.y * 2 } in
        let new_coord = { x = m_coord.x - 1; y = m_coord.y - 1 } in
        let new_res =
          if new_coord.x > 0 && new_coord.y > 0 then [ (m_coord, c); (new_coord, '.') ] else [ (m_coord, c) ]
        in
        aux tail (List.rev_append acc new_res)
  in
  aux matrix_lst []

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
  | None -> failwith "no start character S found"

let fill_start_coord loop =
  let start = { x = 0; y = 0 } in
  let fill_start = Matrix.find_opt start loop in
  assert (fill_start = None);
  { x = 1; y = 1 }

type direction = Top | Right | Bottom | Left

let coord_of_dir dir =
  match dir with
  | Right -> ({ x = 1; y = -1 }, { x = 1; y = 1 })
  | Bottom -> ({ x = 1; y = 1 }, { x = -1; y = 1 })
  | Left -> ({ x = -1; y = -1 }, { x = -1; y = 1 })
  | Top -> ({ x = -1; y = -1 }, { x = 1; y = -1 })

(* starting from origin 0,0 fill the matrix with the augmented resolution *)
let fill_matrix matrix loop =
  let check_direction coord dir =
    let coord_dir = coord_of_dir dir in
    let c1 = coord_dir |> fst |> sum_coord coord in
    let c2 = coord_dir |> snd |> sum_coord coord in
    let v1 = Matrix.find_opt c1 loop in
    let v2 = Matrix.find_opt c2 loop in
    let () = Printf.printf "\nchecking dirs" in
    let are_connected =
      match (v1, v2) with
      | Some uv1, Some uv2 ->
          let () = Printf.printf "\nchecking connections %c %c" uv1 uv2 in
          let are_connected = connected ~res:2 (c1, uv1) (c2, uv2) in
          are_connected
      | _ -> false
    in
    if not are_connected then
      let fill_next = sum_coord (fst coord_dir) (snd coord_dir) |> sum_coord coord in
      let () = Printf.printf " not connected" in
      Some fill_next
    else None
  in
  let rec fill_aux acc coord =
    let val_at = Matrix.find_opt coord matrix in
    if val_at = None then acc
    else
      let directions = [ Top; Right; Bottom; Left ] in
      let rec check_dirs dirs =
        match dirs with
        | [] -> acc
        | dir :: rest -> (
            let dir_coord = check_direction coord dir in
            (*TODO check if dir_coord exist and if it doesnt, dont fill into that dir*)
            match dir_coord with Some d -> fill_aux ((d, 'o') :: acc) d | None -> check_dirs rest)
      in
      check_dirs directions
  in

  let start_coord = fill_start_coord loop in
  fill_aux [ (start_coord, 'o') ] start_coord

let execute' () =
  match start with
  | Some start_coord ->
      let () = Printf.printf "\nstart x:%d y:%d" start_coord.x start_coord.y in
      let s_res = List.length matrix_list in
      let () = Printf.printf "initial resolution %d" s_res in
      let new_matrix_list = inc_resolution matrix_list in
      let () = Printf.printf "\n new resolution matrix size %d" (List.length new_matrix_list) in

      let loop = get_loop matrix (start_coord, 'S') in
      let loop_size = Matrix.to_list loop |> List.length in
      let () = Printf.printf "\nloop size %d " loop_size in

      let new_matrix = Matrix.of_list new_matrix_list in

      let transformed_list = Matrix.to_list loop |> List.map (fun (k, c) -> ({ x = k.x * 2; y = k.y * 2 }, c)) in
      let loop_new = Matrix.of_list transformed_list in

      let fill = fill_matrix new_matrix loop_new in
      let () = List.iter (fun e -> Printf.printf "\n coord %d %d: %c" (fst e).x (fst e).y (snd e)) fill in
      0
  | None -> failwith "no start character S found"
