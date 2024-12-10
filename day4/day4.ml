let input_file = "day4_input.txt"

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

let get_dimensions grid =
  match grid with
  | [||] -> (0, 0)
  | _ -> (Array.length grid, Array.length grid.(0))

let dirs = [|
  (-1, -1); (-1, 0); (-1, 1);
  (0, -1);           (0, 1);
  (1, -1);  (1, 0);  (1, 1)
|]

let count_targets grid i j target_string =
  let (rows, cols) = get_dimensions grid in
  let target_chars = chars_array_of_string target_string in
  let target_len = Array.length target_chars in
  let rec check_dir i j dir_idx target_idx =
    if target_idx = target_len then 1
    else if i < 0 || i >= rows || j < 0 || j >= cols then 0
    else if grid.(i).(j) <> target_chars.(target_idx) then 0
    else check_dir (i + fst dirs.(dir_idx)) (j + snd dirs.(dir_idx)) dir_idx (target_idx + 1)
  in
  let rec count_dirs i j dir_idx =
    if dir_idx = Array.length dirs then 0
    else check_dir i j dir_idx 0 + count_dirs i j (dir_idx + 1)
  in
  count_dirs i j 0

let grid = read_grid_from_file input_file

(* Part 1 *)
let () =
  let target_string = "XMAS" in

  let rows = List.init (Array.length grid) (fun i -> i) in
  let cols = List.init (Array.length grid.(0)) (fun j -> j) in
  let count =
    List.fold_left (fun acc i ->
      List.fold_left (fun acc j ->
        acc + count_targets grid i j target_string
      ) acc cols
    ) 0 rows in
  print_int count;
  print_endline ""

(* Part 2 *)
let range start stop = List.init (stop - start) (fun x -> x + start)

let is_wing arr = 
  arr = ['M'; 'A'; 'S'] || arr = ['S'; 'A'; 'M']

let count_wings grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  
  (* Helper to safely get diagonals around a point *)
  let get_diagonal i j dx dy =
    try [
      grid.(i - 1).(j + dx);
      grid.(i).(j);
      grid.(i + 1).(j + dy)
    ]
    with Invalid_argument _ -> []  (* Return empty list if out of bounds *)
  in
  
  range 1 (height - 1)
  |> List.fold_left (fun count i ->
    count + (range 1 (width - 1)
    |> List.fold_left (fun acc j ->
      if grid.(i).(j) <> 'A' then acc
      else
        let diagonal1 = get_diagonal i j (-1) 1 in (* top-left to bottom-right *)
        let diagonal2 = get_diagonal i j 1 (-1) in (* top-right to bottom-left *)
        if is_wing diagonal1 && is_wing diagonal2 then
          acc + 1
        else acc
    ) 0)
  ) 0

let () =
  let result = count_wings grid in
  print_int result;
  print_newline ()