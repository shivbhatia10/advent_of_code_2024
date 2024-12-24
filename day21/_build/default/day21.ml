(* Prioritise left then down then up then right *)

open! Core

type dir_key =
  | UP
  | RIGHT
  | DOWN
  | LEFT
  | A_DIR

type num_key =
  | ONE
  | TWO
  | THREE
  | FOUR
  | FIVE
  | SIX
  | SEVEN
  | EIGHT
  | NINE
  | ZERO
  | A_NUM

type dir_seq = dir_key list
type num_seq = num_key list

let inputs : num_seq list =
  [ [ FIVE; THREE; NINE; A_NUM ]
  ; [ NINE; SIX; FOUR; A_NUM ]
  ; [ EIGHT; ZERO; THREE; A_NUM ]
  ; [ ONE; FOUR; NINE; A_NUM ]
  ; [ SEVEN; EIGHT; NINE; A_NUM ]
  ]
;;

let test_inputs : num_seq list =
  [ [ ZERO; TWO; NINE; A_NUM ]
  ; [ NINE; EIGHT; ZERO; A_NUM ]
  ; [ ONE; SEVEN; NINE; A_NUM ]
  ; [ FOUR; FIVE; SIX; A_NUM ]
  ; [ THREE; SEVEN; NINE; A_NUM ]
  ]
;;

let dir_key_to_coord = function
  | UP -> 0, 1
  | A_DIR -> 0, 2
  | LEFT -> 1, 0
  | DOWN -> 1, 1
  | RIGHT -> 1, 2
;;

let num_key_to_coord = function
  | SEVEN -> 0, 0
  | EIGHT -> 0, 1
  | NINE -> 0, 2
  | FOUR -> 1, 0
  | FIVE -> 1, 1
  | SIX -> 1, 2
  | ONE -> 2, 0
  | TWO -> 2, 1
  | THREE -> 2, 2
  | ZERO -> 3, 1
  | A_NUM -> 3, 2
;;

let shortest_dir_seq_to_get_num_seq (num_seq : num_seq) : dir_seq =
  let rec go (x, y) = function
    | [] -> []
    | key :: keys ->
      let x', y' = num_key_to_coord key in
      let dx = x' - x in
      let dy = y' - y in
      if dx >= 0 && dy >= 0
      then
        if y = 0 && x' = 3
        then
          List.init dy ~f:(fun _ -> RIGHT)
          @ List.init dx ~f:(fun _ -> DOWN)
          @ [ A_DIR ]
          @ go (x', y') keys
        else
          List.init dx ~f:(fun _ -> DOWN)
          @ List.init dy ~f:(fun _ -> RIGHT)
          @ [ A_DIR ]
          @ go (x', y') keys
      else if dx >= 0 && dy < 0
      then
        List.init (-dy) ~f:(fun _ -> LEFT)
        @ List.init dx ~f:(fun _ -> DOWN)
        @ [ A_DIR ]
        @ go (x', y') keys
      else if dx < 0 && dy >= 0
      then
        List.init (-dx) ~f:(fun _ -> UP)
        @ List.init dy ~f:(fun _ -> RIGHT)
        @ [ A_DIR ]
        @ go (x', y') keys
      else if x = 3 && y' = 0
      then
        List.init (-dx) ~f:(fun _ -> UP)
        @ List.init (-dy) ~f:(fun _ -> LEFT)
        @ [ A_DIR ]
        @ go (x', y') keys
      else
        List.init (-dy) ~f:(fun _ -> LEFT)
        @ List.init (-dx) ~f:(fun _ -> UP)
        @ [ A_DIR ]
        @ go (x', y') keys
  in
  go (num_key_to_coord A_NUM) num_seq
;;

let shortest_dir_seq_to_get_dir_seq (dir_seq : dir_seq) : dir_seq =
  let rec go (x, y) = function
    | [] -> []
    | key :: keys ->
      let x', y' = dir_key_to_coord key in
      let dx = x' - x in
      let dy = y' - y in
      if dx >= 0 && dy >= 0
      then
        List.init dx ~f:(fun _ -> DOWN)
        @ List.init dy ~f:(fun _ -> RIGHT)
        @ [ A_DIR ]
        @ go (x', y') keys
      else if dx >= 0 && dy < 0
      then
        if x = 0 && y' = 0
        then
          List.init dx ~f:(fun _ -> DOWN)
          @ List.init (-dy) ~f:(fun _ -> LEFT)
          @ [ A_DIR ]
          @ go (x', y') keys
        else
          List.init (-dy) ~f:(fun _ -> LEFT)
          @ List.init dx ~f:(fun _ -> DOWN)
          @ [ A_DIR ]
          @ go (x', y') keys
      else if dx < 0 && dy >= 0
      then
        if y = 0 && x' = 0
        then
          List.init dy ~f:(fun _ -> RIGHT)
          @ List.init (-dx) ~f:(fun _ -> UP)
          @ [ A_DIR ]
          @ go (x', y') keys
        else
          List.init (-dx) ~f:(fun _ -> UP)
          @ List.init dy ~f:(fun _ -> RIGHT)
          @ [ A_DIR ]
          @ go (x', y') keys
      else
        List.init (-dy) ~f:(fun _ -> LEFT)
        @ List.init (-dx) ~f:(fun _ -> UP)
        @ [ A_DIR ]
        @ go (x', y') keys
  in
  go (dir_key_to_coord A_DIR) dir_seq
;;

let print_num_seq num_seq =
  List.iter num_seq ~f:(function
    | ONE -> printf "1"
    | TWO -> printf "2"
    | THREE -> printf "3"
    | FOUR -> printf "4"
    | FIVE -> printf "5"
    | SIX -> printf "6"
    | SEVEN -> printf "7"
    | EIGHT -> printf "8"
    | NINE -> printf "9"
    | ZERO -> printf "0"
    | A_NUM -> printf "A");
  printf "\n"
;;

let print_dir_seq dir_seq =
  List.iter dir_seq ~f:(function
    | UP -> printf "^"
    | RIGHT -> printf ">"
    | DOWN -> printf "v"
    | LEFT -> printf "<"
    | A_DIR -> printf "A");
  printf "\n"
;;

exception Invalid_code

let get_numeric_part (code : num_seq) : int =
  List.rev code
  |> List.tl_exn
  |> List.rev
  |> List.fold ~init:0 ~f:(fun acc key ->
    match key with
    | ONE -> (acc * 10) + 1
    | TWO -> (acc * 10) + 2
    | THREE -> (acc * 10) + 3
    | FOUR -> (acc * 10) + 4
    | FIVE -> (acc * 10) + 5
    | SIX -> (acc * 10) + 6
    | SEVEN -> (acc * 10) + 7
    | EIGHT -> (acc * 10) + 8
    | NINE -> (acc * 10) + 9
    | ZERO -> acc * 10
    | A_NUM -> raise Invalid_code)
;;

let apply_n_times f n x =
  List.init n ~f:(fun _ -> f) |> List.fold ~init:x ~f:(fun acc f -> f acc)
;;

let get_complexity (code : num_seq) (num_robots : int) : int =
  let numeric_part = get_numeric_part code in
  let first_dir_seq = code |> shortest_dir_seq_to_get_num_seq in
  let my_dir_seq =
    apply_n_times shortest_dir_seq_to_get_dir_seq num_robots first_dir_seq
  in
  let complexity = numeric_part * List.length my_dir_seq in
  let () =
    print_num_seq code;
    printf "=> ";
    print_dir_seq my_dir_seq;
    printf "\n";
    printf "Numeric part: %d\n" numeric_part;
    printf "Length: %d\n" (List.length my_dir_seq);
    printf "Complexity: %d\n\n" complexity
  in
  complexity
;;

let total_complexity =
  List.fold inputs ~init:0 ~f:(fun acc input -> acc + get_complexity input 2)
;;

let () = printf "Total complexity: %d\n" total_complexity
