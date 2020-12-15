open Lacaml.D
open Bigarray

let to_vec a = Array1.of_array float64 fortran_layout a

let solve segway_mass stick_mass speed length inertia f =
  let g t u = [| [| (f t u.{1} u.{3} u.{2} u.{4}) -. (speed *. u.{3}) +.
                    (stick_mass *. length *. (sin u.{2}) *. u.{4} *. u.{4}) |];
                 [| (-. stick_mass) *. 9.81 *. length *. (sin u.{2}) |] |] in
  let a u = [| [| segway_mass +. stick_mass;
                stick_mass *. length *. (cos u.{2}) |];
             [| stick_mass *. length *. (cos u.{2});
                inertia +. (stick_mass *. length *. length) |] |] in
  let inv_a_g t u =
    let inv_a = Mat.of_array (a u) in
    getri inv_a;
    gemm inv_a (Mat.of_array (g t u)) in
  let ode t (u: vec) (du: vec) =
    let matrix = inv_a_g t u in
    du.{1} <- u.{3};
    du.{2} <- u.{4};
    du.{3} <- matrix.{1, 1};
    du.{4} <- matrix.{2, 1} in
  let u x0 theta0 t =
    let init = to_vec [| x0; theta0; 0.; 0. |] in
    let vec = Odepack.vec(Odepack.lsoda ode init 0. t ~mxstep:3000) in
    [| vec.{1}; vec.{2} |] in
  u