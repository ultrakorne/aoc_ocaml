open Minttea

let day17_inputs = Utils.read_lines "data/advent_17_data_test.txt"

type coord = { x : int; y : int }
type heading = Right | Down | Left | Up

module Grid = Map.Make (struct
  type t = coord

  let compare = compare
end)

let digit_of_char char = int_of_char char - int_of_char '0'

let parse_input input =
  let rec parse_row seq x y lst =
    match seq () with
    | Seq.Nil -> (lst, x)
    | Seq.Cons (s, tail) ->
        let new_lst = if s != '.' then ({ x; y }, digit_of_char s) :: lst else lst in
        parse_row tail (x + 1) y new_lst
  in
  let rec aux line y lst width =
    match line with
    | [] -> (lst, { x = width; y })
    | l :: tail ->
        let seq = String.to_seq l in
        let row, x = parse_row seq 0 y [] in
        aux tail (y + 1) (List.rev_append row lst) (Int.max x width)
  in
  aux input 0 [] 0

let grid_list, grid_size = parse_input day17_inputs
let grid = Grid.of_list grid_list

let coord_of_heading coord heading =
  match heading with
  | Right -> { coord with x = coord.x + 1 }
  | Left -> { coord with x = coord.x - 1 }
  | Down -> { coord with y = coord.y + 1 }
  | Up -> { coord with y = coord.y - 1 }

let valid_headings path =
  let no_backward a b =
    match (a.x, b.x, a.y, b.y) with
    | _, _, ay, by when ay > by -> [ Down; Right; Left ]
    | _, _, ay, by when ay < by -> [ Up; Right; Left ]
    | ax, bx, _, _ when ax > bx -> [ Right; Down; Up ]
    | ax, bx, _, _ when ax < bx -> [ Left; Down; Up ]
    | _ -> failwith "should never happen, cannot move diagonally"
  in

  match path with
  | a :: b :: c :: d :: _ ->
      if a.x <> b.x && b.x <> c.x && c.x <> d.x then [ Up; Down ]
      else if a.y <> b.y && b.y <> c.y && c.y <> d.y then [ Left; Right ]
      else no_backward a b
  | a :: b :: _ -> no_backward a b
  | _ -> [ Right; Down; Left; Up ]

type priority_list = { coord : coord; value : int; path : coord list }

let pathfind_grid grid =
  let path_id path =
    (* let first = List.hd path in
       first.x + (10 * first.y) *)
    match path with
    | a :: b :: c :: d :: _ ->
        a.x + (10 * a.y) + (100 * b.x) + (1000 * b.y) + (10_000 * c.x) + (100_000 * c.y) + (1_000_000 * d.x)
        + (10_000_000 * d.y)
    | a :: b :: c :: _ -> a.x + (10 * a.y) + (100 * b.x) + (1000 * b.y) + (10000 * c.x) + (100000 * c.y)
    | a :: b :: _ -> a.x + (10 * a.y) + (100 * b.x) + (1000 * b.y)
    | a :: _ -> a.x + (10 * a.y)
    | [] -> -1
  in
  let rec aux grid coord node_cost path prio_list already_visited path_candidate =
    let rec check_headings headings acc =
      match headings with
      | [] -> acc
      | h :: rest -> (
          let next_coord = coord_of_heading coord h in
          let next_cell = Grid.find_opt next_coord grid in
          match next_cell with
          | None -> check_headings rest acc
          | Some c -> check_headings rest ((next_coord, c + node_cost) :: acc))
    in
    let new_path = coord :: path in
    let path_candidate =
      if coord = { x = grid_size.x - 1; y = grid_size.y - 1 } then path_candidate @ [ new_path ] else path_candidate
    in
    if List.length path_candidate > 1 then List.hd path_candidate
    else
      (* if coord = { x = grid_size.x - 1; y = grid_size.y - 1 } then new_path *)
      (* else *)
      let headings = valid_headings new_path in
      let next_cells = check_headings headings [] in
      let new_prio_list =
        List.fold_left
          (fun acc x ->
            let c = fst x in
            let v = snd x in
            if List.exists (fun y -> y.coord = c) prio_list then
              List.map
                (fun y -> if y.coord = c && y.value > v then { coord = c; value = v; path = new_path } else y)
                prio_list
            else { coord = c; value = v; path = new_path } :: acc)
          prio_list next_cells
      in
      let new_prio_list = List.sort (fun a b -> compare a.value b.value) new_prio_list in
      List.iter
        (fun x ->
          Printf.printf "prio list: coord: %d %d, value: %d. path size %d node_id %d prev node %d,%d\n" x.coord.x
            x.coord.y x.value (List.length x.path) (path_id x.path) (List.hd x.path).x (List.hd x.path).y)
        new_prio_list;

      let next_node =
        List.find (fun x -> not (List.exists (fun y -> y = path_id (x.coord :: x.path)) already_visited)) new_prio_list
      in
      let node_address = path_id (next_node.coord :: next_node.path) in
      Printf.printf "node address: %d\n" node_address;
      let updated_already_visited = node_address :: already_visited in
      aux grid next_node.coord next_node.value next_node.path new_prio_list updated_already_visited path_candidate
  in

  aux grid { x = 0; y = 0 } 0 [] [] [] []

type model = {
  (* the choices that will be used and whether they are selected or unselected *)
  choices : (string * [ `selected | `unselected ]) list;
  (* the current position of the cursor *)
  cursor : int;
}

let initial_model =
  {
    cursor = 0;
    choices = [ ("Buy empanadas 🥟", `unselected); ("Buy carrots 🥕", `unselected); ("Buy cupcakes 🧁", `unselected) ];
  }

let init _model = Command.Noop

let update event model =
  match event with
  | Event.KeyDown ((Key "q" | Escape), _modifier) -> (model, Command.Quit)
  | Event.KeyDown ((Up | Key "k"), _modifier) ->
      let cursor = if model.cursor = 0 then List.length model.choices - 1 else model.cursor - 1 in
      ({ model with cursor }, Command.Noop)
  | Event.KeyDown ((Down | Key "j"), _modifier) ->
      let cursor = if model.cursor = List.length model.choices - 1 then 0 else model.cursor + 1 in
      ({ model with cursor }, Command.Noop)
  | Event.KeyDown ((Enter | Space), _modifier) ->
      let toggle status = match status with `selected -> `unselected | `unselected -> `selected in
      let choices =
        List.mapi
          (fun idx (name, status) ->
            let status = if idx = model.cursor then toggle status else status in
            (name, status))
          model.choices
      in
      ({ model with choices }, Command.Noop)
  | _ -> (model, Command.Noop)

let red_with_border fmt = Spices.(default |> padding_left 5 |> padding_right 5 |> fg (color "#FF0000") |> build) fmt

let view model =
  (* we create our options by mapping over them *)
  let options =
    model.choices
    |> List.mapi (fun idx (name, checked) ->
           let cursor = if model.cursor = idx then ">" else " " in
           let checked = if checked = `selected then "x" else " " in
           Format.sprintf "%s [%s] %s" cursor checked name)
    |> String.concat "\n"
  in
  let color_text = red_with_border "test" in
  (* and we send the UI for rendering! *)
  Format.sprintf {|
What should we buy at the market?

%s

Press q to quit. %s

  |} options color_text

let execute_mint () =
  let app = Minttea.app ~init ~update ~view () in
  Minttea.start app ~initial_model

let execute () =
  (* let result = pathfind_grid grid in
     Printf.printf "size of grid: %d %d\n" grid_size.x grid_size.y;
     List.iter (fun x -> Printf.printf "path: %d %d\n" x.x x.y) (List.rev result);
     List.fold_left (fun acc x -> acc + Grid.find x grid) 0 result *)
  0
