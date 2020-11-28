open Lacaml.D

let q1 (a: mat) (b: mat) =
  let sol = gemm ~transa:`T a b in
  gesv (gemm ~transa:`T a a) sol;
  sol;;

let deriv (a: num_type -> mat) (b: num_type -> mat)
    (da: num_type -> mat) (db: num_type -> mat) (t: float) =
  let at = a t and dat = da t and bt = b t in
  let x = q1 at bt in
  let sol = gemm ~transa:`T ~beta:1. ~c:(gemm ~transa:`T at
                                 (gemm ~alpha:(-1.) ~beta:1. ~c:(db t) dat x))
             dat (gemm ~alpha:(-.1.) ~beta:1. ~c:bt at x) in
  gesv (gemm ~transa:`T at at) sol;
  sol;;