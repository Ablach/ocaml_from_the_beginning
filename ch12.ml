let rec pp_elems es =
  match es with
    [] -> ""
  | x :: [] -> string_of_int x
  | x :: xs -> string_of_int x ^ ";" ^ pp_elems xs
     
let pp_list l = print_string "[";
                print_string (pp_elems l);
                print_string "]\n"

let rec read_triple () =
  try
    print_string "Please enter the  element:\n";
    let x = read_int in
    print_string "Please enter the second element:\n";
    let y = read_int in
    print_string "Please enter the third element:\n";
    let z = read_int in
    (x,y,z)
  with
    Failure _ -> print_string "Input must be integers, please try again\n";
                 read_triple  ()

exception Neg

let rec read_entries len =
  if len = 0 then [] else
    try
      print_string "Enter key then press enter\n";
      let i = read_int () in
      print_string "Enter value then press enter\n";
      let name = read_line () in
      (i,name) :: read_entries (len - 1)
    with
      Failure _ -> print_string "Invalid int, please try again.\n";
                   read_entries len

let rec read_dict () =
  try
    print_string "Please enter the length of the dictionary\n";
    let len = read_int () in
    if len < 0 then raise Neg else read_entries len
  with
    Failure _ | Neg -> print_string "Please enter a valid number\n";
                       read_dict ()

let rec intersperse c l =
  match l with
    [] -> []
  | [x] -> [x]
  | x :: xs -> x :: c :: intersperse c xs

let rec make_row n =
  match n with
    0 -> []
  | _ -> make_row (n - 1) @ [n]

let rec make_matrix n i row =
  if i > n then [] else
    map (( * ) i) row :: make_matrix n (i + 1) row 

let n_table_as_str n =
  foldr (^) ""
    (foldr (@) []
       (intersperse ["\n"]
          (map (intersperse "\t")
             (map (map string_of_int)
                (make_matrix n 1 (make_row n))))))

let print_times_to_file n file =
  let handle = open_out file in
  output_string handle (n_table_as_str n);
  close_out

let rec count_lines h =
  try
    let line = input_line h in
    1 + count_lines h
  with
    End_of_file -> 0

let get_count f =
  let h = open_in f in
  let x = count_lines h in
  close_in h;
  x

let rec cp_contents ha hb =
  try
    let line = input_line ha in
    output_string hb line;
    output_string hb "\n";
    cp_contents ha hb
  with
    End_of_file -> ()


let cp_file a b =
  try
    let ha = open_in a in
    let hb = open_out b in
    cp_contents ha hb;
    close_in ha;
    close_out hb;
  with
    Sys_error _ -> print_string "problem opening file";
                   ()
  
