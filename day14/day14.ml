module Types = struct
  type pos = int * int
  type vel = int * int
end

module IO = struct
  open Types
  open Re

  let read_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    let () = close_in ch in
    s

  let digit_pattern = compile (seq [ group (seq [ opt (char '-'); rep1 digit ]) ])

  let get_nums (file : string) =
    all digit_pattern file |> List.map (fun group -> Group.get group 1) |> List.map int_of_string

  let rec get_chunks = function
    | px :: py :: vx :: vy :: tail -> ((px, py), (vx, vy)) :: get_chunks tail
    | _ -> []

  let get_robots filename = read_file filename |> get_nums |> get_chunks
end

module Main = struct
  let range a b =
    let rec helper start = if start >= b then [] else start :: helper (start + 1) in
    helper a

  let pos_after_seconds pos vel s n m =
    let px, py = pos in
    let vx, vy = vel in
    let px' = (px + (s * vx)) mod n in
    let py' = (py + (s * vy)) mod m in
    let px'' = if px' < 0 then px' + n else px' in
    let py'' = if py' < 0 then py' + m else py' in
    (px'', py'')

  let move_robots robots s n m =
    List.map (fun (pos, vel) -> (pos_after_seconds pos vel s n m, vel)) robots

  let get_robot_positions = List.map fst

  let positions_in_each_quadrant positions n m =
    positions
    |> List.map (fun (px, py) ->
           let q1 = if px < n / 2 && py < m / 2 then 1 else 0 in
           let q2 = if px > n / 2 && py < m / 2 then 1 else 0 in
           let q3 = if px < n / 2 && py > m / 2 then 1 else 0 in
           let q4 = if px > n / 2 && py > m / 2 then 1 else 0 in
           (q1, q2, q3, q4))
    |> List.fold_left
         (fun (acc1, acc2, acc3, acc4) (q1, q2, q3, q4) ->
           (acc1 + q1, acc2 + q2, acc3 + q3, acc4 + q4))
         (0, 0, 0, 0)

  let get_safety_factor quadrant_counts =
    let q1, q2, q3, q4 = quadrant_counts in
    q1 * q2 * q3 * q4

  let print_robots robots n m =
    let positions = get_robot_positions robots in
    let grid = Array.make_matrix n m '.' in
    let () =
      List.iter
        (fun (px, py) ->
          grid.(px).(py) <- '#';
          ())
        positions
    in
    Array.iter
      (fun row ->
        Array.iter (fun cell -> print_char cell) row;
        print_endline "")
      grid
end

open IO
open Main

(* let input_file = "day14_test.txt"
   let n, m = (11, 7) *)
let input_file = "day14_input.txt"
let n, m = (101, 103)

(* Part 1 *)
let robots = get_robots input_file
let seconds = 100
let moved_robots = move_robots robots seconds n m
let positions = get_robot_positions moved_robots
let quadrant_counts = positions_in_each_quadrant positions n m
let safety_factor = get_safety_factor quadrant_counts
let () = Printf.printf "Safety factor: %d\n" safety_factor

(* Part 2 *)

let rec loop i min_safety_factor curr_robots =
  let next_robots = move_robots curr_robots 1 n m in
  let positions = get_robot_positions next_robots in
  let quadrant_counts = positions_in_each_quadrant positions n m in
  let safety_factor = get_safety_factor quadrant_counts in
  let min_safety_factor' =
    if safety_factor < min_safety_factor then
      let () =
        print_endline "";
        Printf.printf "Time = %d" i;
        print_robots next_robots n m;
        print_endline ""
      in
      safety_factor
    else min_safety_factor
  in
  loop (i + 1) min_safety_factor' next_robots

let () = loop 1 max_int robots
