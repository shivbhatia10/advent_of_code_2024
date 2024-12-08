let input_file = "day6_test.txt"

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let split_lines s =
  String.split_on_char '\n' s
  |> List.filter (fun line -> String.length line > 0)

let chars_array_of_string s =
  List.init (String.length s) (String.get s)
  |> Array.of_list

let read_grid_from_file filename =
  read_file filename
  |> split_lines
  |> List.map chars_array_of_string
  |> Array.of_list

let guard_symbol_to_direction symbol =
  match symbol with
  | '^' -> (-1, 0)
  | '>' -> (0, 1)
  | 'v' -> (1, 0)
  | '<' -> (0, -1)
  | _ -> failwith "Invalid direction symbol"

let reverse_direction (direction: int*int) =
  (-fst direction, -snd direction)

let rotate_right (direction: int*int) = 
  (snd direction, -fst direction)
let rotate_left (direction: int*int) = 
  direction |> rotate_right |> reverse_direction

let is_within_bounds pos n m =
  let i, j = pos in
  i >= 0 && i < n && j >= 0 && j < m


let get_guard_position_and_direction (grid: char array array): (int*int)*(int*int) =
  let n = Array.length grid in
  let m = Array.length grid.(0) in
  let rec find_guard_position_and_direction i j =
    if i = n then failwith "Guard not found"
    else if j = m then find_guard_position_and_direction (i+1) 0
    else
      match grid.(i).(j) with
      | '^' | '>' | 'v' | '<' -> ((i, j), guard_symbol_to_direction grid.(i).(j))
      | _ -> find_guard_position_and_direction i (j+1)
  in
  find_guard_position_and_direction 0 0

let (+) (tuple1: int*int) (tuple2: int*int)
  = ((fst tuple1) + (fst tuple2), (snd tuple1) + (snd tuple2))

let print_visited =
  Hashtbl.iter (fun (p1, p2) (d1, d2) -> Printf.printf "(%d, %d) -> (%d, %d)\n" p1 p2 d1 d2)

let count_guard_positions (grid: char array array) (pos: int*int) (direction: int*int): int =
  (* 
  Returns -1 if guard is caught in a loop, 
  else number of positions visited before leaving the board
  *)
  let n = Array.length grid in
  let m = Array.length grid.(0) in
  let visited = Hashtbl.create 1000 in
  let () = print_visited visited in
  let rec helper curr_pos curr_direction =
    match Hashtbl.find_all visited pos with
    | x::_-> false
    | [] ->
      let () = Hashtbl.add visited curr_pos curr_direction in
      let next_pos = curr_pos + curr_direction in
      match is_within_bounds next_pos n m with
      | true -> (match grid.(fst next_pos).(snd next_pos) with
        | '#' -> helper curr_pos (rotate_right curr_direction)
        | _ -> helper next_pos curr_direction)
      | false -> true
    in
  let () = print_visited visited in
  match helper pos direction with
  | true -> Hashtbl.length visited
  | false -> -1

  (* helper pos direction *)

let print_grid grid =
  Array.iter (fun row -> 
    Array.iter (fun c -> print_char c) row;
    print_newline ()) grid


let grid = read_grid_from_file input_file

let guard_position, guard_direction = get_guard_position_and_direction grid

let () =
  (* print_grid grid; *)
  print_int (count_guard_positions grid guard_position guard_direction)


(* Part 1 *)