open! Core

let filename = "day19_input.txt"

let file =
  let ch = In_channel.create filename in
  let s = In_channel.input_all ch in
  let () = In_channel.close ch in
  s
;;

let filter_empty = List.filter ~f:(fun x -> String.length x > 0)
let lines = String.split_lines file |> filter_empty
let patterns = List.hd_exn lines |> String.split_on_chars ~on:[ ','; ' ' ] |> filter_empty
let towels = List.tl_exn lines
let string_head s = String.get s 0
let string_tail s = String.slice s 1 (String.length s)

let get_possible_ways (towel : string) pattern_set : int =
  let memo =
    Hashtbl.create
      (module struct
        type t = string * string

        let hash = Hashtbl.hash

        let compare (s1, s2) (t1, t2) =
          match String.compare s1 t1 with
          | 0 -> String.compare s2 t2
          | c -> c
        ;;

        let sexp_of_t (s1, s2) = Sexp.List [ Sexp.Atom s1; Sexp.Atom s2 ]
      end)
  in
  let rec helper curr_towel acc =
    match Hashtbl.find memo (curr_towel, acc) with
    | Some result -> result
    | None ->
      let res =
        if String.length curr_towel = 0
        then if Set.mem pattern_set acc then 1 else 0
        else (
          let new_acc = String.append acc (String.make 1 (string_head curr_towel)) in
          let next = helper (string_tail curr_towel) new_acc in
          if Set.mem pattern_set new_acc
          then next + helper (string_tail curr_towel) ""
          else next)
      in
      let () = Hashtbl.add_exn memo ~key:(curr_towel, acc) ~data:res in
      res
  in
  helper towel ""
;;

let pattern_set = Set.of_list (module String) patterns

let possible_ways_for_towels =
  List.map towels ~f:(fun towel -> get_possible_ways towel pattern_set)
;;

let int_sum = List.fold ~init:0 ~f:( + )

let res1 =
  possible_ways_for_towels |> List.map ~f:(fun x -> if x > 0 then 1 else 0) |> int_sum
;;

let res2 = possible_ways_for_towels |> int_sum
let () = Printf.printf "Part 1: %d, Part 2: %d\n" res1 res2
