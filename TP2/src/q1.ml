open Lacaml.D

let q1 (a: mat) (b: mat) =
(** Returns the solution of the linear equation A^t * A * X = A^t * b. *)
  let sol = gemm ~transa:`T a b in
  gesv (gemm ~transa:`T a a) sol;
  sol;;

let display (vector: (num_type, 'a, 'b) Bigarray.Array2.t) (n: int) =
(** Output :
      Unit expression that prints each float in the matrix with a precision
      of 10 decimals and go to the next line after each float printed.
*)
  for j = 1 to n do
    Printf.printf "%.10f\n" vector.{j, 1};
  done;;

let () =
(** Main function, the inputs come from the command line.

    Output :
      Print on the standard output, the matrix solution
      of the equation A^t * A * X = A^t * b.
*)
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
  display (q1 a b) n;;