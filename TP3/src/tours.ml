open Lacaml.D
open Bigarray

type vec = (float, float64_elt, fortran_layout)Array1.t

let to_vec a = Array1.of_array float64 fortran_layout a

let maximum a b =
  if a > b then a else b

let root_solve segway_mass stick_mass speed length inertia f =
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
  let root_search t (u: vec) (v: vec) =
    v.{1} <- u.{4} +. (0. *. t) in
  let init = to_vec [| 0.; 3.2; 0.; 0. |] in
  let y = Odepack.lsodar ~atol:10e-5 ~mxstep:3000 ~g:root_search ~ng:1
          ode init 0. 0. in
  let liste = [|abs_float (Odepack.vec y).{2}|] in
  Odepack.advance y ~time:10.;
  liste.(0) <- maximum liste.(0) (abs_float (Odepack.vec y).{2});
  while Odepack.has_root y do
    Odepack.advance y ~time:10.;
    liste.(0) <- maximum liste.(0) (abs_float (Odepack.vec y).{2});
  done;
  liste.(0)

let () =
  let f omega t x dx theta dtheta = 0.3 *. (sin (omega *. t)) +.
                              (0. *. (x +. dx +. theta +. dtheta)) in
  let u omega = root_solve 0.5 0.2 0.1 0.3 0.006 (f omega) in
  Printf.printf "%.8f\n" (u 6.)