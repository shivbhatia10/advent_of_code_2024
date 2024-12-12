type pos = int * int

module PosCompare = struct
  type t = pos

  let compare (x1, y1) (x2, y2) = match compare x1 x2 with 0 -> compare y1 y2 | c -> c
end

module PosSet = Set.Make (PosCompare)

let print_pos_set set =
  let () =
    print_char '{';
    PosSet.iter (fun (x, y) -> Printf.printf "(%d,%d), " x y) set;
    print_string "}\n"
  in
  ()

let string_of_char = String.make 1
let input_file = "day12_test.txt"

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  let () = close_in ch in
  s

let split_lines s = String.split_on_char '\n' s |> List.filter (fun line -> String.length line > 0)
let chars_array_of_string s = List.init (String.length s) (String.get s) |> Array.of_list

let read_grid_from_file filename =
  read_file filename |> split_lines |> List.map chars_array_of_string |> Array.of_list

let print_grid (grid : char array array) =
  Array.iter
    (fun row ->
      Array.iter
        (fun cell ->
          print_char cell;
          print_char ' ')
        row;
      print_endline "")
    grid

let get_dim grid = (Array.length grid, Array.length grid.(0))

let range a b =
  let rec helper start = if start >= b then [] else start :: helper (start + 1) in
  helper a

let is_in_range x a b = a <= x && x < b
let grid = read_grid_from_file input_file
let dirs = [ (0, 1); (0, -1); (1, 0); (-1, 0) ]
let n, m = get_dim grid
let rotate_right di dj = (dj, -di)
let rotate_left di dj = (-dj, di)

let is_valid_perimeter_side i j di dj =
  let ni, nj = (i + di, j + dj) in
  (not (is_in_range ni 0 n)) || (not (is_in_range nj 0 m)) || not (grid.(i).(j) = grid.(ni).(nj))

let is_new_perimeter_side i j di dj visited =
  let rdi, rdj = rotate_right di dj in
  let ldi, ldj = rotate_left di dj in
  let rec helper x y dx dy =
    let nx, ny = (x + dx, y + dy) in
    match is_in_range nx 0 n && is_in_range ny 0 m && grid.(i).(j) = grid.(nx).(ny) with
    | true ->
        if PosSet.mem (nx, ny) visited && is_valid_perimeter_side nx ny di dj then true
        else helper nx ny dx dy
    | false -> false
  in
  let left_side_already_added = helper i j rdi rdj in
  let right_side_already_added = helper i j ldi ldj in
  let res =
    is_valid_perimeter_side i j di dj && not (left_side_already_added || right_side_already_added)
  in
  (* let () =
       if res then
         Printf.printf "%s at (%d, %d) in direction (%d, %d) is valid: %B\n"
           (String.make 1 grid.(i).(j))
           i j di dj res
       else ()
     in *)
  res

let get_perimeter i j =
  List.fold_left
    (fun acc (di, dj) ->
      acc + match is_valid_perimeter_side i j di dj with true -> 1 | false -> 0)
    0 dirs

let get_unique_sides i j visited =
  List.fold_left
    (fun acc (di, dj) ->
      acc + match is_new_perimeter_side i j di dj visited with true -> 1 | false -> 0)
    0 dirs

let get_valid_neighbours i j visited =
  List.map (fun (di, dj) -> (i + di, j + dj)) dirs
  |> List.filter (fun (x, y) -> is_in_range x 0 n && is_in_range y 0 m)
  |> List.filter (fun (x, y) -> grid.(i).(j) = grid.(x).(y))
  |> List.filter (fun (x, y) -> not (PosSet.mem (x, y) visited))

let rec dfs1 i j (visited : PosSet.t) =
  match PosSet.mem (i, j) visited with
  | true -> (0, 0, visited)
  | false ->
      let visited' = PosSet.add (i, j) visited in
      let valid_neighbours = get_valid_neighbours i j visited' in
      let perimeter = get_perimeter i j in
      let area = 1 in
      List.fold_left
        (fun (curr_area, curr_perimeter, curr_visited) (x, y) ->
          let next_area, next_perimeter, next_visited = dfs1 x y curr_visited in
          (curr_area + next_area, curr_perimeter + next_perimeter, next_visited))
        (area, perimeter, visited') valid_neighbours

let rec dfs2 i j (visited : PosSet.t) =
  match PosSet.mem (i, j) visited with
  | true -> (0, 0, visited)
  | false ->
      let visited' = PosSet.add (i, j) visited in
      let valid_neighbours = get_valid_neighbours i j visited' in
      let sides = get_unique_sides i j visited' in
      let area = 1 in
      List.fold_left
        (fun (curr_area, curr_perimeter, curr_visited) (x, y) ->
          let next_area, next_perimeter, next_visited = dfs2 x y curr_visited in
          (curr_area + next_area, curr_perimeter + next_perimeter, next_visited))
        (area, sides, visited') valid_neighbours

let fencing dfs =
  let rec process_grid i j (visited : PosSet.t) =
    if i = n then []
    else if j == m then process_grid (i + 1) 0 visited
    else
      let region_area, region_perimeter, visited' = dfs i j visited in
      match region_area > 0 || region_perimeter > 0 with
      | true -> (grid.(i).(j), region_area, region_perimeter) :: process_grid i (j + 1) visited'
      | false -> process_grid i (j + 1) visited'
  in
  process_grid 0 0 PosSet.empty

let get_price fencing =
  List.fold_left (fun acc (_, area, perimeter) -> acc + (area * perimeter)) 0 fencing

let () =
  fencing dfs1 |> get_price |> print_int;
  print_endline ""

let () =
  fencing dfs2
  |> List.iter (fun (letter, area, perimeter) ->
         Printf.printf "%s: (area=%d, perimeter=%d)\n" (string_of_char letter) area perimeter)

let () =
  fencing dfs2 |> get_price |> print_int;
  print_endline ""

(* let rec get_all_pos_in_region i j (visited : PosSet.t) =
     match PosSet.mem (i, j) visited with
     | true -> visited
     | false ->
         let visited' = PosSet.add (i, j) visited in
         let valid_neighbours = get_valid_neighbours i j visited' in
         List.fold_left
           (fun curr_visited (x, y) -> get_all_pos_in_region x y curr_visited)
           visited' valid_neighbours

   let regions =
     let rec process_grid i j (visited : PosSet.t) =
       if i = n then []
       else if j == m then process_grid (i + 1) 0 visited
       else if PosSet.mem (i, j) visited then process_grid i (j + 1) visited
       else
         let all_pos_in_region = get_all_pos_in_region i j PosSet.empty in
         let visited' = PosSet.union visited all_pos_in_region in
         all_pos_in_region :: process_grid i (j + 1) visited'
     in
     process_grid 0 0 PosSet.empty

   let bulk_price_of_fencing =
     List.fold_left
       (fun acc region ->
         let region_area = PosSet.cardinal region in
         let region_sides =
           PosSet.fold (fun (i, j) acc' -> acc' + get_unique_sides i j region) region 0
         in
         acc + (region_area * region_sides))
       0 regions *)
