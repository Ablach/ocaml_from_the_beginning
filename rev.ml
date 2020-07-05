let flip h_in h_out = 
  let str = ref "" in
  try
    while true do
      let line = input_line h_in in
      str := line ^ "\n" ^ !str
    done;
    !str
  with
    End_of_file -> !str
  
let () =
  try
    begin match Sys.argv with
      [|_;file_in;file_out|] ->
       let h_in = open_in file_in in
       let h_out = open_out file_out in
       let str = flip h_in h_out in
       output_string h_out str;
       close_in h_in;
       close_out h_out
    | _ ->
       print_string "Usage: rev <input file> <output file>\n"
    end
  with
    e -> print_string  "An error occured\n"

