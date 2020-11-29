open Lacaml.D

open Bigarray
type vect = (float, float64_elt, fortran_layout) Array2.t

let a (t: float) =
  (* Matrix A :  (  sin t   ,  2t   )
                 (     t    ,  e^t  )
                 (  atan t  ,   t   )
      Output :
        Returns the matrix where each component has been evaluated in t.
  *)
  let mat_a = Mat.create 3 2 in
  mat_a.{1, 1} <- sin t;
  mat_a.{1, 2} <- 2. *. t;
  mat_a.{2, 1} <- t;
  mat_a.{2, 2} <- exp t;
  mat_a.{3, 1} <- (atan t);
  mat_a.{3, 2} <- t;
  mat_a;;

let b (t: float) =
  (* Matrix b :  (        t       )
                 (      t**2      )
                 (  1/(1 + t**2)  )
      Output :
        Returns the matrix where each component has been evaluated in t.
  *)
  let mat_b = Mat.create 3 1 in
  mat_b.{1, 1} <- t;
  mat_b.{2, 1} <- t ** 2.;
  mat_b.{3, 1} <- 1. /. (1. +. t ** 2.);
  mat_b;;

let da (t: float) =
  (* Matrix derived from A :  (     cos t     ,   2   )
                              (       1       ,  e^t  )
                              (  1/(1 + t**2) ,   1   )
      Output :
        Returns the matrix where each component has been evaluated in t.
  *)
  let d_mat_a = Mat.create 3 2 in
  d_mat_a.{1, 1} <- cos t;
  d_mat_a.{1, 2} <- 2.;
  d_mat_a.{2, 1} <- 1.;
  d_mat_a.{2, 2} <- exp t;
  d_mat_a.{3, 1} <- 1. /. (1. +. t ** 2.);
  d_mat_a.{3, 2} <- 1.;
  d_mat_a;;

let db (t: float) =
  (* Matrix bMatrix derived from b :  (            1           )
                                      (           2*t          )
                                      (  -2*t/((1 + t**2)**2)  )
      Output :
        Returns the matrix where each component has been evaluated in t.
  *)
  let d_mat_b = Mat.create 3 1 in
  d_mat_b.{1, 1} <- 1.;
  d_mat_b.{2, 1} <- 2. *. t;
  d_mat_b.{3, 1} <- (-.2.) *. t /. ((1. +. (t ** 2.)) ** 2.);
  d_mat_b;;

let q3 (t: float) =
  (* Returns an approximation of the derivative of x(t)
      for functions A and b which are :
        Matrix A: (  sin t   ,  2t   )
                  (     t    ,  e^t  )
                  (  atan t  ,   t   )

        Matrix b : (        t       )
                   (      t**2      )
                   (  1/(1 + t**2)  )
  *)
  Q2.deriv a b da db t;;

let display (vector: vect) (n: int) =
  (* Output :
        Unit expression that prints each float in the matrix with a precision
        of 10 decimals and go to the next line after each float printed.
  *)
  for j = 1 to n do
    Printf.printf "%.10f\n" vector.{j, 1};
  done;;

let () =
  (* Main function, the inputs come from the command line.

      Output :
        Print on the standard output, the matrix solution
        of the equation A^t * A * X = A^t * b.
  *)
  let t = Float.of_string(Sys.argv.(1)) in
  display (q3 t) 2 ;;