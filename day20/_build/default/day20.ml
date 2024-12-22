open! Core

let filename = "day20_input.txt"

let file =
  let ch = In_channel.create filename in
  let s = In_channel.input_all ch in
  let () = In_channel.close ch in
  s
;;

let grid = file |> String.split_lines |> Array.of_list |> Array.map ~f:String.to_array
let n, m = Array.length grid, Array.length grid.(0)

let rec find i j c =
  if i >= n
  then raise (Invalid_argument "Could not find S in grid")
  else if j >= m
  then find (i + 1) 0 c
  else if Char.equal grid.(i).(j) c
  then i, j
  else find i (j + 1) c
;;

let sx, sy = find 0 0 'S'
let ex, ey = find 0 0 'E'

let tbl =
  Hashtbl.create
    (module struct
      type t = int * int

      let compare (x1, y1) (x2, y2) =
        match Int.compare x1 x2 with
        | 0 -> Int.compare y1 y2
        | c -> c
      ;;

      let hash (x, y) = Hashtbl.hash (x, y)

      let sexp_of_t (x, y) =
        Sexp.List [ Sexp.Atom (Int.to_string x); Sexp.Atom (Int.to_string y) ]
      ;;
    end)
;;

let dirs = [ 0, 1; 0, -1; 1, 0; -1, 0 ]

let calculate_distances i j =
  let rec helper i j prev_i prev_j =
    if Char.equal grid.(i).(j) 'E'
    then Hashtbl.add_exn tbl ~key:(i, j) ~data:0
    else (
      let next_step =
        List.map dirs ~f:(fun (dx, dy) ->
          let x, y = i + dx, j + dy in
          if x >= 0
             && x < n
             && y >= 0
             && y < m
             && Char.( <> ) grid.(x).(y) '#'
             && (x <> prev_i || y <> prev_j)
          then (
            let () = helper x y i j in
            x, y)
          else -1, -1)
        |> List.filter ~f:(fun (a, _) -> a <> -1)
        |> List.hd_exn
      in
      let dist = 1 + Hashtbl.find_exn tbl next_step in
      Hashtbl.add_exn tbl ~key:(i, j) ~data:dist)
  in
  helper i j (-1) (-1)
;;

let () = calculate_distances sx sy
let end_dist = Hashtbl.find_exn tbl (ex, ey)
let () = printf "End dist: %d\n" end_dist
let start_dist = Hashtbl.find_exn tbl (sx, sy)
let () = printf "Start dist: %d\n\n" start_dist

(* A cheat is when you jump over a wall to another . *)
type cheat =
  { x : int
  ; y : int
  ; dx : int
  ; dy : int
  }

let find_cheats_at_point i j : cheat list =
  let valid_cheats =
    List.map dirs ~f:(fun (dx, dy) ->
      let wall_x, wall_y = i + dx, j + dy in
      let x, y = wall_x + dx, wall_y + dy in
      if x >= 0
         && x < n
         && y >= 0
         && y < m
         && Char.( <> ) grid.(i).(j) '#'
         && Char.equal grid.(wall_x).(wall_y) '#'
         && Char.( <> ) grid.(x).(y) '#'
         && Hashtbl.find_exn tbl (x, y) < Hashtbl.find_exn tbl (i, j)
      then { x = i; y = j; dx; dy }
      else { x = -1; y = -1; dx = -1; dy = -1 })
    |> List.filter ~f:(fun cheat -> cheat.x <> -1)
  in
  valid_cheats
;;

let all_valid_cheats_as_list =
  List.init n ~f:(fun i -> List.init m ~f:(fun j -> find_cheats_at_point i j))
  |> List.concat
  |> List.concat
;;

(* Should be 44 for test *)
let () = printf "Number of cheats: %d\n\n" (List.length all_valid_cheats_as_list)

let time_saved_by_cheat cheat =
  let dist_from_start = start_dist - Hashtbl.find_exn tbl (cheat.x, cheat.y) in
  let dist_to_end =
    Hashtbl.find_exn tbl (cheat.x + (2 * cheat.dx), cheat.y + (2 * cheat.dy))
  in
  start_dist - (dist_from_start + 2 + dist_to_end)
;;

let times_saved_by_cheats =
  List.map all_valid_cheats_as_list ~f:(fun cheat -> time_saved_by_cheat cheat)
;;

let num_cheats_that_save_at_least_100 =
  List.count times_saved_by_cheats ~f:(fun x -> x >= 100)
;;

let () =
  printf "Number of cheats that save at least 100: %d\n" num_cheats_that_save_at_least_100
;;

(* let () =
   List.iter all_valid_cheats_as_list ~f:(fun cheat ->
   let time_saved = time_saved_by_cheat cheat in
   let () = printf "Time saved: %d\n" time_saved in
   ())
   ;; *)
