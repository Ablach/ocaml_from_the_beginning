let rec string_in str line =
  if String.length str > String.length line then false else
    if (String.sub line 0 (String.length str)) = str then true
    else string_in str (String.sub line 1 (String.length line - 1))


let search h str =
  let lines = ref "" in
  try
    while true do
      let line = input_line h in
      if string_in str line then (lines := !lines ^ "\n" ^ line ^ "\n")
    done;
      !lines
  with
    End_of_file -> !lines

let () =
  try
    begin match Sys.argv with
      [|_;file;phrase|] ->
      let h = open_in file in
      let str = phrase in
      let s = search h str in
      print_string s;
      close_in h
    | _ -> print_string "Usage: search_file_for <file> <phrase>"
    end
  with
    e -> print_string "An error occurred"
