let sum_arr arr =
  let total = ref 0 in
  for count = 0 to Array.length arr
  do
    total := !total + arr.(count)
  done;
  !total

let rev_arr arr =
  if arr <> [||] then
  for i = 0 to Array.length arr / 2 do
    let tmp = arr.(i) in
    arr.(i) <- arr.(Array.length arr - i - 1);
    arr.(Array.length arr - i - 1) <- tmp;
  done;
  arr
    
let table n =
  let matrix = Array.make n [||] in
  for i = 0 to n - 1 do
    let row = Array.make n 1 in
    for j = 0 to n - 1 do
      row.(j) <- (i + 1) * (j + 1)
    done;
      matrix.(i) <- row;
  done;
    matrix

let upper c = if int_of_char c >= 97
                 && int_of_char c <= 122
              then char_of_int (int_of_char c - 32)
              else c

let lower c = if int_of_char c >= 65
                 && int_of_char c <= 90 
              then char_of_int (int_of_char c + 32)
              else c

let print_histogram arr =
  print_string "Character frequencies: ";
  print_newline ();
  for x = 0 to 255 do
    if arr.(x) > 0 then
      begin
        print_string "For character '";
        print_char (char_of_int x);
        print_string "' the count is ";
        print_int arr.(x);
        print_string ".\n";
      end
  done



let channel_statistics in_chan =
  let lines = ref 0 in
  let characters = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  let histogram = Array.make 256 0 in
  try while true do
        let line = input_line in_chan in
        lines := !lines + 1;
        characters := !characters + String.length line;
        String.iter
          (fun c ->
            match c with
              '.' | '?' | '!' -> sentences := !sentences + 1
              | ' ' -> words := !words + 1
              | _ -> ())
          line;
        String.iter
          (fun c ->
            let i = int_of_char c in
            histogram.(i) <- histogram.(i) + 1)
          line;
        words := !words + 1;
      done
  with
    End_of_file -> print_string "There were ";
                   print_int !lines;
                   print_string " making up ";
                   print_int !characters;
                   print_string " characters with ";
                   print_int !words;
                   print_string " words in ";
                   print_int !sentences;
                   print_string " sentences.";
                   print_newline ();
                   print_histogram histogram
