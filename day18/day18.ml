open! Core

module Memory = struct
  type byte_pos =
    { x : int
    ; y : int
    }

  type memory =
    { n : int
    ; m : int
    ; bytes : char array array
    }

  let string_of_char = String.make 1

  let get_blank_memory n m : memory =
    { n; m; bytes = Array.make_matrix ~dimx:n ~dimy:m '.' }
  ;;

  let set (memory : memory) (byte_pos : byte_pos) (c : char) : memory =
    let { n; m; bytes } = memory in
    let new_bytes =
      Array.mapi
        ~f:(fun i row ->
          Array.mapi
            ~f:(fun j cell -> if i = byte_pos.x && j = byte_pos.y then c else cell)
            row)
        bytes
    in
    { n; m; bytes = new_bytes }
  ;;

  (* let dirs = [ 0, 1; 0, -1; 1, 0; -1, 0 ]
  let dir_to_char = function
    | 0, 1 -> '>'
    | 0, -1 -> '<'
    | 1, 0 -> 'v'
    | -1, 0 -> '^'
    | _ -> raise (Invalid_argument "Invalid dx and dy")

  let bfs (p:byte_pos) (t:byte_pos) (memory: memory) : (memory * int) option =
    let rec loop (d: Deque) =
      match Deque.peek_front d with
      | None -> None
      | Some (curr_dist, curr_p, curr_memory) ->
        if curr_p=t then Some (curr_memory, curr_dist)
        else
          let new_memory = set memory p 'O' in
          let next_points = List.map ~f(fun (dx, dy) -> ) dirs
        
      in
    loop (Deque.of_array [(0, p, memory)])

  let shortest_path_to_exit (memory : memory) : (memory * int) option =
    let p = {x=0;y=0} in
    let t = {x=memory.n - 1; y=memory.m - 1} in
    bfs p t memory
  ;; *)

  let corrupt_all (memory : memory) (list_byte_pos : byte_pos list) : memory =
    List.fold list_byte_pos ~init:memory ~f:(fun curr_memory byte_pos ->
      set curr_memory byte_pos '#')
  ;;

  let first_n_bytes n (list_byte_pos : byte_pos list) : byte_pos list =
    let rec helper i ls =
      if i = 0
      then []
      else (
        match ls with
        | [] -> []
        | x :: xs -> x :: helper (i - 1) xs)
    in
    helper n list_byte_pos
  ;;

  let print_memory (memory : memory) =
    Printf.printf "Height: %d, Width: %d\n" memory.n memory.m;
    Array.iter memory.bytes ~f:(fun row ->
      row
      |> Array.to_list
      |> List.map ~f:string_of_char
      |> String.concat ~sep:""
      |> Printf.printf "%s\n")
  ;;
end

module IO = struct
  open Re
  open Memory

  let read_file filename =
    let ch = In_channel.create filename in
    let s = In_channel.input_all ch in
    let () = In_channel.close ch in
    s
  ;;

  let digit_pattern = compile (seq [ group (seq [ opt (char '-'); rep1 digit ]) ])

  let get_nums (file : string) =
    all digit_pattern file
    |> List.map ~f:(fun group -> Group.get group 1)
    |> List.map ~f:int_of_string
  ;;

  let get_byte_positions nums : byte_pos list =
    let rec helper = function
      | x :: y :: rest -> { x; y } :: helper rest
      | _ -> []
    in
    helper nums
  ;;
end

open Memory
open IO

let input_file = "day18_input.txt"
let n, m = 71, 71
let byte_positions = input_file |> read_file |> get_nums |> get_byte_positions
let memory = get_blank_memory n m
let () = print_memory memory
let corrupted_memory = corrupt_all memory (first_n_bytes 1024 byte_positions)
let () = print_memory corrupted_memory
