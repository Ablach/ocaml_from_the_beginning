try
  begin match Sys.argv with
    [|_;filename|] ->
    let stats = Textstat.stats_from_file filename in
    print_string "Words: ";
    print_int (Textstat.words stats);
    print_newline ();
    print_string "Characters: ";
    print_int (Textstat.characters stats);
    print_newline ();
    print_string "Sentences: ";
    print_int (Textstat.sentences stats);
    print_newline ();
    print_string "Lines: ";
    print_int (Textstat.lines stats);
    print_newline ();
        let h = Textstat.c_hist stats in
            for i = 0 to 256 do
              if h.(i) != 0 then (
                print_char (char_of_int i);
                print_string " was used ";
                print_int h.(i);
                print_string " times.\n"; )
            done
          | _ ->
             print_string "Usage: stats <filename>";
             print_newline ()
  end
with
  e -> print_string "An error occured ";
       print_string (Printexc.to_string e);
       print_newline ();
       exit 1
