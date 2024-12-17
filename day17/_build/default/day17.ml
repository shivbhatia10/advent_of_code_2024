module ComputerTypes = struct
  type registers =
    { a : int
    ; b : int
    ; c : int
    }

  type program =
    { pointer : int
    ; instructions : int array
    }

  type computer =
    { registers : registers
    ; program : program
    }
end

module IO = struct
  open Re
  open ComputerTypes

  exception BadProgramFormat

  let read_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    let () = close_in ch in
    s
  ;;

  let digit_pattern = compile (seq [ group (seq [ opt (char '-'); rep1 digit ]) ])

  let get_nums (file : string) =
    all digit_pattern file
    |> List.map (fun group -> Group.get group 1)
    |> List.map int_of_string
  ;;

  let get_registers_and_program (lst : int list) : ComputerTypes.computer =
    match lst with
    | a :: b :: c :: program_list ->
      { registers = { a; b; c }
      ; program = { pointer = 0; instructions = Array.of_list program_list }
      }
    | _ -> raise BadProgramFormat
  ;;

  let get_computer_from_filename input_file =
    read_file input_file |> get_nums |> get_registers_and_program
  ;;
end

module Computer = struct
  open ComputerTypes

  exception InvalidComboOperand

  let print_computer (computer : computer) =
    let { a; b; c } = computer.registers in
    let { pointer; instructions } = computer.program in
    Printf.printf "(A: %d, B: %d, C: %d)\n" a b c;
    Printf.printf "Program: ";
    Array.iter (fun instruction -> Printf.printf "%d " instruction) instructions;
    Printf.printf "at position %d\n" pointer
  ;;

  let literal (operand : int) = operand

  let combo (operand : int) (registers : registers) =
    let { a; b; c } = registers in
    match operand with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 2
    | 3 -> 3
    | 4 -> a
    | 5 -> b
    | 6 -> c
    (* Should never happen in a valid program *)
    | _ -> raise InvalidComboOperand
  ;;

  (* 0 *)
  let adv operand (registers : registers) : registers * int option =
    let { a; b; c } = registers in
    let combo_operand = combo operand registers in
    let denominator = int_of_float (float_of_int 2 ** float_of_int combo_operand) in
    { a = a / denominator; b; c }, None
  ;;

  (* 1 *)
  let bxl operand (registers : registers) : registers * int option =
    let { a; b; c } = registers in
    let literal_operand = literal operand in
    { a; b = b lxor literal_operand; c }, None
  ;;

  (* 2 *)
  let bst operand (registers : registers) : registers * int option =
    let { a; b = _; c } = registers in
    let combo_operand = combo operand registers in
    { a; b = combo_operand mod 8; c }, None
  ;;

  (* 4 *)
  let bxc _operand (registers : registers) : registers * int option =
    let { a; b; c } = registers in
    { a; b = b lxor c; c }, None
  ;;

  (* 5 *)
  let out operand (registers : registers) : registers * int option =
    let combo_operand = combo operand registers in
    registers, Some (combo_operand mod 8)
  ;;

  (* 6 *)
  let bdv operand (registers : registers) : registers * int option =
    let { a; b = _; c } = registers in
    let combo_operand = combo operand registers in
    let denominator = int_of_float (float_of_int 2 ** float_of_int combo_operand) in
    { a; b = a / denominator; c }, None
  ;;

  (* 7 *)
  let cdv operand (registers : registers) : registers * int option =
    let { a; b; c = _ } = registers in
    let combo_operand = combo operand registers in
    let denominator = int_of_float (float_of_int 2 ** float_of_int combo_operand) in
    { a; b; c = a / denominator }, None
  ;;

  let apply_non_jump_instruction (computer : computer) instruction : computer * int option
    =
    let { registers; program } = computer in
    let { pointer; instructions } = program in
    let operand = Array.get instructions (pointer + 1) in
    let new_registers, maybe_output = instruction operand registers in
    ( { registers = new_registers; program = { pointer = pointer + 2; instructions } }
    , maybe_output )
  ;;

  let apply_jump_instruction (computer : computer) : computer * int option =
    let { registers; program } = computer in
    let { pointer; instructions } = program in
    let operand = Array.get instructions (pointer + 1) in
    if registers.a = 0
    then { registers; program = { pointer = pointer + 2; instructions } }, None
    else
      ( (let literal_operand = literal operand in
         { registers; program = { pointer = literal_operand; instructions } })
      , None )
  ;;

  let maybe_step (computer : computer) : (computer * int option) option =
    (* let () = print_computer computer in *)
    let { registers = _; program } = computer in
    let { pointer; instructions } = program in
    let len_instructions = Array.length instructions in
    if pointer <= len_instructions - 2
    then (
      let instruction = Array.get program.instructions program.pointer in
      match instruction with
      | 0 -> Some (apply_non_jump_instruction computer adv)
      | 1 -> Some (apply_non_jump_instruction computer bxl)
      | 2 -> Some (apply_non_jump_instruction computer bst)
      | 3 -> Some (apply_jump_instruction computer)
      | 4 -> Some (apply_non_jump_instruction computer bxc)
      | 5 -> Some (apply_non_jump_instruction computer out)
      | 6 -> Some (apply_non_jump_instruction computer bdv)
      | 7 -> Some (apply_non_jump_instruction computer cdv)
      | _ -> None)
    else None
  ;;

  let get_output computer : int list =
    let rec helper curr_computer =
      match maybe_step curr_computer with
      | None -> []
      | Some (next_computer, maybe_output) ->
        (match maybe_output with
         | None -> helper next_computer
         | Some output -> output :: helper next_computer)
    in
    let res = helper computer in
    let () = print_newline () in
    res
  ;;

  let does_a_reproduce_instructions a (program : program) : bool =
    let init_registers = { a; b = 0; c = 0 } in
    let init_computer = { registers = init_registers; program } in
    let n = Array.length program.instructions in
    let rec helper i curr_computer =
      if i >= n
      then true
      else (
        match maybe_step curr_computer with
        (* Program terminated without reproducing output *)
        | None -> false
        | Some (next_computer, maybe_output) ->
          (match maybe_output with
           | None -> helper i next_computer
           | Some output ->
             if program.instructions.(i) <> output
             then false
             else helper (i + 1) next_computer))
    in
    helper 0 init_computer
  ;;

  let find_smallest_a_to_reproduce_instructions (program : program) : int =
    let rec helper i =
      match does_a_reproduce_instructions i program with
      | true -> i
      | false -> helper (i + 1)
    in
    helper 0
  ;;
end

open IO
open Computer

let input_file = "day17_input.txt"
let computer = get_computer_from_filename input_file
let () = print_computer computer
let output = get_output computer
let () = List.map string_of_int output |> String.concat "," |> print_endline
let smallest_a_to_reproduce = find_smallest_a_to_reproduce_instructions computer.program

let () =
  Printf.printf "Smallest A to reproduce instructions: %d\n" smallest_a_to_reproduce
;;
