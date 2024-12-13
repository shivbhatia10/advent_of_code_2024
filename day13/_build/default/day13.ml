[@@@warning "-3"]
#require "re";;
[@@@warning "+3"]
open Re

let input_file = "day13_input.txt"

module Utils = struct
  let string_of_char = String.make 1

  let range a b =
    let rec helper start = if start >= b then [] else start :: helper (start + 1) in
    helper a

  let read_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    let () = close_in ch in
    s

  let split_list_on_empty =
    let rec helper acc current = function
      | [] -> List.rev (List.rev current :: acc)
      | "" :: t -> helper (List.rev current :: acc) [] t
      | h :: t -> helper acc (h :: current) t
    in
    helper [] []

  let print_para para = List.iter (fun x -> print_endline x) para
  let get_paras s = String.split_on_char '\n' s |> split_list_on_empty
  let paras = get_paras (read_file input_file)
  let remove_last_char s = String.sub s 0 (String.length s - 1)
end

module Types = struct
  open Int64

  type button_delta = { x : t; y : t }
  type target = { x : t; y : t }
  type problem = { a : button_delta; b : button_delta; t : target }
end

module IO = struct
  open Types
  open Utils
  open Int64

  let extract_num split term =
    let pieces = String.split_on_char split term in
    List.nth pieces 1 |> of_string

  let extract_button_delta line : button_delta =
    let terms = String.split_on_char ' ' line in
    let x_term, y_term = (List.nth terms 2 |> remove_last_char, List.nth terms 3) in
    { x = extract_num '+' x_term; y = extract_num '+' y_term }

  let extract_target line : target =
    let terms = String.split_on_char ' ' line in
    let x_term, y_term = (List.nth terms 1 |> remove_last_char, List.nth terms 2) in
    { x = extract_num '=' x_term; y = extract_num '=' y_term }

  let offset = 10000000000000L
  let add_offset_to_target t = { x = add t.x offset; y = add t.y offset }

  let process_para para : problem =
    let first, second, third = (List.nth para 0, List.nth para 1, List.nth para 2) in
    { a = extract_button_delta first; b = extract_button_delta second; t = extract_target third }

  let process_para2 para : problem =
    let first, second, third = (List.nth para 0, List.nth para 1, List.nth para 2) in
    {
      a = extract_button_delta first;
      b = extract_button_delta second;
      t = extract_target third |> add_offset_to_target;
    }
end

module AoC = struct
  open Utils
  open Types
  open IO
  open Int64

  let problems1 = List.map process_para paras
  let problems2 = List.map process_para2 paras

  let get_maybe_solution (prob : problem) =
    let { a; b; t } = prob in
    let num1 = sub (mul b.y t.x) (mul b.x t.y) in
    let num2 = sub (mul a.x t.y) (mul a.y t.x) in
    let det = sub (mul a.x b.y) (mul a.y b.x) in
    if det = zero || rem num1 det <> zero || rem num2 det <> zero then None
    else Some (div num1 det, div num2 det)

  let deoptionalize (l : (Int64.t * Int64.t) option list) =
    let rec deopt acc = function
      | [] -> List.rev acc
      | None :: tl -> deopt acc tl
      | Some x :: tl -> deopt (x :: acc) tl
    in
    deopt [] l

  let get_tokens (a_count, b_count) = add (mul 3L a_count) b_count
  let get_all_valid_solutions problems = List.map get_maybe_solution problems |> deoptionalize

  let fewest_tokens problems =
    let valid_sols = get_all_valid_solutions problems in
    List.fold_left (fun acc best_sol -> Int64.add acc (get_tokens best_sol)) Int64.zero valid_sols
end

open AoC

let () =
  fewest_tokens problems1 |> Int64.to_string |> print_endline;
  fewest_tokens problems2 |> Int64.to_string |> print_endline
