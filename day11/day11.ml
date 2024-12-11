let input_file = "day11_input.txt"

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let split_lines s = String.split_on_char '\n' s |> List.filter (fun line -> String.length line > 0)

let vals =
  read_file input_file |> split_lines |> List.hd |> String.split_on_char ' '
  |> List.map int_of_string

let rec count_stones (value, blinks_remaining) =
  let cache = Hashtbl.create 50 in
  try Hashtbl.find cache (value, blinks_remaining)
  with Not_found ->
    let result =
      if blinks_remaining = 0 then 1
      else if value = 0 then count_stones (1, blinks_remaining - 1)
      else
        let str_val = string_of_int value in
        let str_len = String.length str_val in
        match str_len mod 2 with
        | 0 ->
            let half_len = str_len / 2 in
            let a = String.sub str_val 0 half_len |> int_of_string in
            let b = String.sub str_val half_len half_len |> int_of_string in
            count_stones (a, blinks_remaining - 1) + count_stones (b, blinks_remaining - 1)
        | _ -> count_stones (value * 2024, blinks_remaining - 1)
    in
    Hashtbl.add cache (value, blinks_remaining) result;
    result

let get_stones vals blinks = List.fold_left (fun acc v -> acc + count_stones (v, blinks)) 0 vals

let () =
  get_stones vals 25 |> print_int;
  print_endline ""

let () =
  get_stones vals 75 |> print_int;
  print_endline ""
