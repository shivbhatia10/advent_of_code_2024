let input_file = "day10_test.txt"

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
  |> Array.map (Array.map (int_of_char))
