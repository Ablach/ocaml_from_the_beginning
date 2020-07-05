let () =
  let t = Sys.time() in
  for x = 0 to 200000 do () done;
  Printf.printf "Time taken %fs\n" (Sys.time() -. t)
