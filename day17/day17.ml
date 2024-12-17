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

  exception BadProgramFormatException

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
    | _ -> raise BadProgramFormatException
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
    Printf.printf "\n(A: %d, B: %d, C: %d)\n" a b c;
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
    | _ -> raise InvalidComboOperand
  ;;

  let adv operand (registers : registers) : registers * int option =
    let { a; b; c } = registers in
    let combo_operand = combo operand registers in
    { a = Int.shift_right a combo_operand; b; c }, None
  ;;

  let bxl operand (registers : registers) : registers * int option =
    let { a; b; c } = registers in
    let literal_operand = literal operand in
    { a; b = b lxor literal_operand; c }, None
  ;;

  let bst operand (registers : registers) : registers * int option =
    let { a; b = _; c } = registers in
    let combo_operand = combo operand registers in
    { a; b = combo_operand mod 8; c }, None
  ;;

  let bxc _operand (registers : registers) : registers * int option =
    let { a; b; c } = registers in
    { a; b = b lxor c; c }, None
  ;;

  let out operand (registers : registers) : registers * int option =
    let combo_operand = combo operand registers in
    registers, Some (combo_operand mod 8)
  ;;

  let bdv operand (registers : registers) : registers * int option =
    let { a; b = _; c } = registers in
    let combo_operand = combo operand registers in
    { a; b = Int.shift_right a combo_operand; c }, None
  ;;

  let cdv operand (registers : registers) : registers * int option =
    let { a; b; c = _ } = registers in
    let combo_operand = combo operand registers in
    { a; b; c = Int.shift_right a combo_operand }, None
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
end

module ManualCompiler = struct
  let step_output a =
    let mod_eight = a mod 8 in
    let b_eight = mod_eight lxor 4 in
    let c_eight = Int.shift_right a (mod_eight lxor 1) in
    b_eight lxor c_eight mod 8
  ;;

  let range n =
    let rec helper i = if i >= n then [] else i :: helper (i + 1) in
    helper 0
  ;;

  let get_all_solutions target_list : int list =
    let rev_targets = List.rev target_list in
    let rec helper prefix = function
      | [ t ] ->
        range 8
        |> List.map (fun x -> prefix + x)
        |> List.filter (fun y -> t = step_output y)
      | t :: ts ->
        range 8
        |> List.filter (fun x -> t = step_output (prefix + x))
        |> List.map (fun x -> helper (Int.shift_left (prefix + x) 3) ts)
        |> List.concat
      | [] -> []
    in
    helper 0 rev_targets
  ;;
end

open IO
open Computer

let input_file = "day17_input.txt"
let computer = get_computer_from_filename input_file
let () = print_computer computer
let output = get_output computer

let () =
  Printf.printf "Output: ";
  List.map string_of_int output |> String.concat "," |> print_endline
;;

let sols = ManualCompiler.get_all_solutions (Array.to_list computer.program.instructions)
let min_sol = List.fold_left Int.min Int.max_int sols
let () = Printf.printf "Smallest solution: %d" min_sol

let test_output =
  get_output { registers = { a = min_sol; b = 0; c = 0 }; program = computer.program }
;;

let () =
  Printf.printf "Output on smallest solution: ";
  List.map string_of_int test_output |> String.concat "," |> print_endline
;;
