module IO = struct
  open Re

  let read_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    let () = close_in ch in
    s

  let digit_pattern = compile (seq [ group (rep1 digit) ])

  let get_nums (file : string) =
    all digit_pattern file |> List.map (fun group -> Group.get group 1) |> List.map of_string

  let nums_to_problem (ax, ay, bx, by, tx, ty) =
    { a = { x = ax; y = ay }; b = { x = bx; y = by }; t = { x = tx; y = ty } }

  let rec get_chunks = function
    | ax :: ay :: bx :: by :: tx :: ty :: tail -> (ax, ay, bx, by, tx, ty) :: get_chunks tail
    | _ -> []

  let get_problems filename =
    read_file filename |> get_nums |> get_chunks |> List.map nums_to_problem
end
