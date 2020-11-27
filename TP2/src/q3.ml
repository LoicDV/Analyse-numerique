open Lacaml.D

let a t =
  let mat_a = Mat.create 3 2 in
  mat_a.{1, 1} <- sin t;
  mat_a.{1, 2} <- 2. *. t;
  mat_a.{2, 1} <- t;
  mat_a.{2, 2} <- exp t;
  mat_a.{3, 1} <- (atan t);
  mat_a.{3, 2} <- t;
  mat_a;;

let b t =
  let mat_b = Mat.create 3 1 in
  mat_b.{1, 1} <- t;
  mat_b.{2, 1} <- t ** 2.;
  mat_b.{3, 1} <- 1. /. (1. +. t ** 2.);
  mat_b;;

let da t =
  let d_mat_a = Mat.create 3 2 in
  d_mat_a.{1, 1} <- cos t;
  d_mat_a.{1, 2} <- 2.;
  d_mat_a.{2, 1} <- 1.;
  d_mat_a.{2, 2} <- exp t;
  d_mat_a.{3, 1} <- 1. /. (1. +. t ** 2.);
  d_mat_a.{3, 2} <- 1.;
  d_mat_a;;

let db t =
  let d_mat_b = Mat.create 3 1 in
  d_mat_b.{1, 1} <- 1.;
  d_mat_b.{2, 1} <- 2. *. t;
  d_mat_b.{3, 1} <- (-.2.) *. t /. ((1. +. (t ** 2.)) ** 2.);
  d_mat_b;;

let q3 t =
  Q2.deriv a b da db t;;

let display vector n =
  for j = 1 to n do
    Printf.printf "%.10f\n" vector.{j, 1};
  done;;

let () =
  let t = Float.of_string(Sys.argv.(1)) in
  display (q3 t) 2 ;;