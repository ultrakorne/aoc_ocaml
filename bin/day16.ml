let day16_inputs = Utils.read_lines "data/advent_16_data.txt"

type coord = { x : int; y : int }
type heading = Right | Down | Left | Up
type cell = { cell_type : char; headings : heading list }

let coord_from_heading coord heading =
  match heading with
  | Right -> { coord with x = coord.x + 1 }
  | Left -> { coord with x = coord.x - 1 }
  | Down -> { coord with y = coord.y + 1 }
  | Up -> { coord with y = coord.y - 1 }

module Grid = Map.Make (struct
  type t = coord

  let compare = compare
end)

let parse_input input =
  let rec parse_row seq x y grid_lst =
    match seq () with
    | Seq.Nil -> grid_lst
    | Seq.Cons (s, tail) ->
        let new_lst = ({ x; y }, { cell_type = s; headings = [] }) :: grid_lst in
        parse_row tail (x + 1) y new_lst
  in
  let rec aux line y grid_lst =
    match line with
    | [] -> grid_lst
    | l :: tail ->
        let seq = String.to_seq l in
        let row = parse_row seq 0 y [] in
        aux tail (y + 1) (List.rev_append row grid_lst)
  in
  aux input 0 []

let grid_list = parse_input day16_inputs
let grid = Grid.of_list grid_list

let reflect heading char =
  match (char, heading) with
  | '\\', Up -> [ Left ]
  | '\\', Down -> [ Right ]
  | '\\', Left -> [ Up ]
  | '\\', Right -> [ Down ]
  | '/', Up -> [ Right ]
  | '/', Right -> [ Up ]
  | '/', Left -> [ Down ]
  | '/', Down -> [ Left ]
  | '-', (Down | Up) -> [ Right; Left ]
  | '|', (Left | Right) -> [ Up; Down ]
  | '.', _ | '-', (Right | Left) | '|', (Up | Down) -> [ heading ]
  | _ -> failwith "reflect all case should be covered"

let light_beam grid start_coord start_heading =
  let light_cell cell heading =
    let new_headings = reflect heading cell.cell_type in
    let missing_headings = List.filter (fun x -> not (List.mem x cell.headings)) new_headings in
    if List.is_empty missing_headings then (None, cell)
    else (Some missing_headings, { cell with headings = missing_headings @ cell.headings })
  in

  let rec aux grid coord heading =
    (* propagate the light, from coord to a list of headings *)
    let cell = Grid.find_opt coord grid in
    match cell with
    | None -> grid
    | Some c -> (
        let new_headings, new_cell = light_cell c heading in
        match new_headings with
        | None -> grid
        | Some heading_list ->
            let new_grid =
              Grid.update coord (function None -> failwith "all cell should exist" | Some _ -> Some new_cell) grid
            in
            List.fold_left
              (fun acc h ->
                let new_coord = coord_from_heading coord h in
                aux acc new_coord h)
              new_grid heading_list)
  in

  aux grid start_coord start_heading

let print_grid grid w h =
  let rec aux x y =
    if x >= w then
      let () = Printf.printf "\n" in
      aux 0 (y + 1)
    else if y >= h then Printf.printf "\n"
    else
      let sym = Grid.find_opt { x; y } grid in
      match sym with
      | None ->
          Printf.printf " ";
          aux (x + 1) y
      | Some c ->
          if List.length c.headings > 0 then
            let () = Printf.printf "#" in
            aux (x + 1) y
          else
            let () = Printf.printf "%c" c.cell_type in
            aux (x + 1) y
  in
  Printf.printf "\n";
  aux 0 0

let execute () =
  let t = Sys.time () in
  let lit_grid = light_beam grid { x = 0; y = 0 } Right in
  let result = Grid.fold (fun _ v acc -> if List.length v.headings > 0 then acc + 1 else acc) lit_grid 0 in
  Printf.printf "\nExecution time: %fs\n" (Sys.time () -. t);
  result

let execute' () =
  let all_starts heading =
    let rec aux i heading max =
      let aux' heading x y =
        let lit_grid = light_beam grid { x; y } heading in
        let result = Grid.fold (fun _ v acc -> if List.length v.headings > 0 then acc + 1 else acc) lit_grid 0 in
        aux (i + 1) heading (Int.max max result)
      in
      if i >= 110 then max
      else
        match heading with
        | Right -> aux' heading 0 i
        | Left -> aux' heading 109 i
        | Down -> aux' heading i 0
        | Up -> aux' heading i 109
    in
    aux 0 heading 0
  in
  List.fold_left (fun acc h -> Int.max acc (all_starts h)) 0 [ Right; Left; Down; Up ]
