let q1 a =
  a;;

let display array =
  Array.iter (Array.iter (Printf.printf "%.8f\n")) array;;

let () = 
  let m = int_of_string Sys.argv.(1) and n = int_of_string Sys.argv.(2) in
  let a = Array.make_matrix m n 0. in
  for i = 0 to (m - 1) do
    for j = 0 to (n - 1) do
      a.(i).(j) <- float_of_string Sys.argv.(3 + (i * n) + j)
    done;
  done;
  display (q1 a);;