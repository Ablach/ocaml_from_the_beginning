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
