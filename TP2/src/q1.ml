open Lacaml.D
open Lacaml.Io
open Format

let q1 a b =
  let sol = gemm ~transa:`T a b in
  gesv (gemm ~transa:`T a a) sol;
  sol;;

let () =
  let m = int_of_string Sys.argv.(1) and n = int_of_string Sys.argv.(2) in
  let a = Mat.create m n and b = Mat.create m 1 in
  for i = 0 to (m - 1) do
    for j = 0 to (n - 1) do
      a.{i + 1, j + 1} <- (Float.of_string Sys.argv.(3 + (i * n) + j))
    done;
  done;
  for i = 0 to (m - 1) do
    b.{i + 1, 1} <- (Float.of_string Sys.argv.(3 + (m * n) + i))
  done;
  printf "%a@\n" pp_fmat (q1 a b);;