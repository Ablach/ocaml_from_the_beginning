(* Text statistics *)

type stats = int * int * int * int * int array

let lines (l,_,_,_,_) = l
let characters (_,c,_,_,_) = c
let words (_,_,w,_,_) = w
let sentences (_,_,_,s,_) = s
let c_hist (_,_,_,_,c) = c


let stats_from_channel in_channel =
  let lines = ref 0 in
  let characters = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  let hist = Array.make 256 0 in
  try
    while true do
      let line = input_line in_channel in
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
          hist.(i) <- hist.(i) + 1)
        line
    done;
    (0,0,0,0, [||]) (* Making the type agree *)
  with
    End_of_file -> (!lines, !characters, !words, !sentences, hist)

let stats_from_file fname =
  let channel = open_in fname in
  let result = stats_from_channel channel in
  close_in channel;
  result
