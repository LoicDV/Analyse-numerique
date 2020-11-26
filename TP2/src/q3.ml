open Lacaml.D
open Lacaml.Io
open Format

let assign_mat mat_a mat_b t =
  mat_a.{1, 1} <- sin t;
  mat_a.{1, 2} <- 2. *. t;
  mat_a.{2, 1} <- t;
  mat_a.{2, 2} <- exp t;
  mat_a.{3, 1} <- (atan t);
  mat_a.{3, 2} <- t;
  mat_b.{1, 1} <- t;
  mat_b.{2, 1} <- t ** 2.;
  mat_b.{3, 1} <- 1. /. (1. +. t ** 2.);
  ;;

let assign_mat_deriv d_mat_a d_mat_b t =
  d_mat_a.{1, 1} <- cos t;
  d_mat_a.{1, 2} <- 2.;
  d_mat_a.{2, 1} <- 1.;
  d_mat_a.{2, 2} <- exp t;
  d_mat_a.{3, 1} <- 1. /. (1. +. t ** 2.);
  d_mat_a.{3, 2} <- 1.;
  d_mat_b.{1, 1} <- 1.;
  d_mat_b.{2, 1} <- 2. *. t;
  d_mat_b.{3, 1} <- -2. *. t /. (1. +. t ** 2.) ** 2.;
  ;;

let q3 t =
  let mat_a = Mat.create 3 2 and mat_b = Mat.create 3 1 and
      d_mat_a = Mat.create 3 2 and d_mat_b = Mat.create 3 1 in
    assign_mat mat_a mat_b t;
    assign_mat_deriv d_mat_a d_mat_b t;
    q2.q2 mat_a mat_b d_mat_a d_mat_b t;;

let () =
  let t = Float.of_string(Sys.argv.(1)) in
  Printf.printf "%.10f" (q3 t);;