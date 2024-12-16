module IO = struct
  exception CharNotFoundException

  let read_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    let () = close_in ch in
    s

  let split_lines s =
    String.split_on_char '\n' s |> List.filter (fun line -> String.length line > 0)

  let chars_array_of_string s = List.init (String.length s) (String.get s) |> Array.of_list

  let read_grid_from_file filename =
    read_file filename |> split_lines |> List.map chars_array_of_string |> Array.of_list

  let string_of_char = String.make 1

  let read_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    let () = close_in ch in
    s

  let split_lines s =
    String.split_on_char '\n' s |> List.filter (fun line -> String.length line > 0)

  let chars_array_of_string s = List.init (String.length s) (String.get s) |> Array.of_list

  let read_grid_from_file filename =
    read_file filename |> split_lines |> List.map chars_array_of_string |> Array.of_list

  let maybe_find_char_in_grid c grid =
    let n, m = (Array.length grid, Array.length grid.(0)) in
    let rec find i j =
      if i >= n then None
      else if j >= m then find (i + 1) 0
      else match grid.(i).(j) = c with true -> Some (i, j) | false -> find i (j + 1)
    in
    find 0 0

  let find_char_in_grid_or_raise c grid =
    match maybe_find_char_in_grid c grid with
    | Some (x, y) -> (x, y)
    | None -> raise CharNotFoundException

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
end

open IO

let string_of_char = String.make 1
let input_file = "day12_test.txt"
let grid = read_grid_from_file input_file
let sx, sy = find_char_in_grid_or_raise 'S' grid
let ex, ey = find_char_in_grid_or_raise 'E' grid
let dx, dy = (0, 1)
