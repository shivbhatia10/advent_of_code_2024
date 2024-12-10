let input_file = "day10_input.txt"

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
  |> Array.map (Array.map (String.make 1))
  |> Array.map (Array.map (int_of_string))

let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      print_int cell;
      print_string " "
    ) row;
    print_newline ()
  ) grid


let dirs = [
  (0, 1);
  (0, -1);
  (1, 0);
  (-1, 0);
]

let rec ways_to_peak grid i j =
  match grid.(i).(j) with
  | 9 -> 1
  | x -> List.fold_left (fun acc (di, dj) ->
    let ni = i + di in
    let nj = j + dj in
    if ni >= 0 && ni < Array.length grid && nj >= 0 && nj < Array.length grid.(0) && grid.(ni).(nj) = (x+1) then
      acc + ways_to_peak grid ni nj
    else
      acc
  ) 0 dirs

let range a b =
  let rec helper start =
    if start >= b then []
    else  start :: helper (start + 1)
  in
  helper a

let res = 
  let grid = read_grid_from_file input_file in
  let n = Array.length grid in
  let m = Array.length grid.(0) in
  List.fold_left (fun acc i -> 
    List.fold_left (fun acc2 j ->
      match grid.(i).(j) with
      | 0 -> acc2 + (ways_to_peak grid i j)
      | _ -> acc2
    ) 0 (range 0 m) + acc
  ) 0 (range 0 n)

let () = print_int res
