let parse_number_pairs filename =
  let process_line line =
    match String.split_on_char ' ' line |> List.filter ((<>) "") with
    | [x; y] -> Some (int_of_string x, int_of_string y)
    | _ -> None
  in
  let chan = open_in filename in
  let rec read_lines nums1 nums2 =
    try
      let line = input_line chan in
      match process_line line with
      | Some (x, y) -> read_lines (x :: nums1) (y :: nums2)
      | None -> read_lines nums1 nums2
    with End_of_file ->
      close_in chan;
      (List.rev nums1, List.rev nums2)
  in
  read_lines [] []

let a, b = parse_number_pairs "day1_input.txt" ;;
let sorted_a = List.sort compare a;;
let sorted_b = List.sort compare b;;

let part1res = List.fold_left2 (fun acc x y -> acc + abs (x - y)) 0 sorted_a sorted_b;;
print_int part1res;;

