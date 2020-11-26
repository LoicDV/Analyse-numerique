open Lacaml.D

let q1 a b =
  let sol = gemm ~transa:`T a b in
  gesv (gemm ~transa:`T a a) sol;
  sol;;

let q2 a b da db t =
  let at = a t and dat = da t and bt = b t in
  let x = q1 at bt in
  let sol = gemm ~transa:`T ~c:(gemm ~transa:`T at
                                 (gemm ~alpha:(-1.) ~c:(db t) dat x))
             dat (gemm ~alpha:(-.1.) ~beta:1. ~c:bt at x) in
  gesv (gemm ~transa:`T at at) sol;
  sol;;