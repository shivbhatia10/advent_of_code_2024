

module Types = struct
  open Int64

  type button_delta = { x : t; y : t }
  type target = { x : t; y : t }
  type problem = { a : button_delta; b : button_delta; t : target }
end

module IO = struct
  open Types
  open Int64
  open Re

  let read_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    let () = close_in ch in
    s

  let digit_pattern = compile (seq [
    group (rep1 digit)
  ])

  let get_nums (file: string) =
    all digit_pattern file
    |> List.map (fun group -> Group.get group 1)
    |> List.map of_string

  let nums_to_problem (ax, ay, bx, by, tx, ty) =
    {
      a = {x=ax;y=ay};
      b={x=bx;y=by};
      t={x=tx;y=ty}
    }

  let rec get_chunks = function
    | ax::ay::bx::by::tx::ty::tail -> (ax,ay,bx,by,tx,ty)::(get_chunks tail)
    | _ -> []
  
  let get_problems filename =
    read_file filename
    |> get_nums
    |> get_chunks
    |> List.map nums_to_problem
end

module AoC = struct
  open Types
  open IO
  open Int64

  let add_offset_to_target offset t = { x = add t.x offset; y = add t.y offset }
  let add_offset_to_problem offset p =
    {
      a=p.a;b=p.b;t = (add_offset_to_target offset p.t)
    }

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

open IO
open AoC

let input_file = "day13_input.txt"

let problems1 = get_problems input_file
let problems2 = List.map (add_offset_to_problem 10000000000000L) problems1

let () =
  fewest_tokens problems1 |> Int64.to_string |> print_endline;
  fewest_tokens problems2 |> Int64.to_string |> print_endline
