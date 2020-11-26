open Lacaml.D
open Lacaml.Io
open Format

let assign_mat mat_a mat_b t =
  mat_a.{1, 1} <- sin t;
  mat_a.{1, 2} <- 2. *. t;
  mat_a.{2, 1} <- t;
  mat_a.{2, 2} <- exp t;
  mat_a.{3, 1} <- (tan t) ** (-1.); (* Pas tangeante mais arctangeante *)
  mat_a.{3, 2} <- t;
  mat_b.{1, 1} <- t;
  mat_b.{2, 1} <- t ** 2.;
  mat_b.{3, 1} <- 1. /. (1. +. t ** 2.);
  ;;

let q3 t =
  let mat_a = Mat.create 3 2 and mat_b = Mat.create 3 1 in
    assign_mat mat_a mat_b t;
    q2 mat_a mat_b;;

let () =
  let t = Float.of_string(Sys.argv.(1)) in
  Printf.printf "%.10f" (q3 t);;