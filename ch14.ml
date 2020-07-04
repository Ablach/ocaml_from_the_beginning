let round n = let trunc = floor n in
              if n -. trunc <= 0.5
              then trunc
              else trunc +. 1.

let equi_point (x,y) (x',y') =
  (((x +. x') /. 2.),((y +. y') /. 2.))

let rec deconstruct n = if n > 0.
                        then (floor n, n -. floor n)
                        else let x,y = deconstruct (-. n) in
                             (-. x, y)

let star n =
  let i = int_of_float (floor (n *. 50.)) in
  let i' = if i = 50 then 49 else i in
  for x = 0 to i' do print_char ' ' done;
  print_string "*\n"
                    
let plot f start stop step =
  let count = ref start in
  while !count <= stop do
    star (f !count);
    count := !count +. step
  done
