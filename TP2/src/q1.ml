open Lacaml.D
open Lacaml.Io
open Format

let q1 a b =
  let a = Mat.of_array a in
  let b = Mat.of_array b in
  let ata = gemm ~transa:`T a a in
  let atb = gemm ~transa:`T a b in
  gesv ata atb;
  printf "@[%a@]@\n" pp_fmat atb;;


let () = 
  let m = int_of_string Sys.argv.(1) and n = int_of_string Sys.argv.(2) in
  let a = Array.make_matrix m n 0. and b = Array.make_matrix m 1 0. in
  for i = 0 to (m - 1) do
    for j = 0 to (n - 1) do
      a.(i).(j) <- Float.of_string Sys.argv.(3 + (i * n) + j)
    done;
  done;
  for i = 0 to (m - 1) do
    b.(i).(0) <- Float.of_string Sys.argv.(3 + (m * n) + i)
  done;
  q1 a b;;