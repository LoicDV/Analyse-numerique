open Curve_sampling
open Printf
open Bigarray

type vec = (float, float64_elt, fortran_layout)Array1.t

let to_vec a = Array1.of_array float64 fortran_layout a

let () =
  let fh = open_out "graphs.gp" in
  fprintf fh "set terminal pngcairo\n\
              set grid\n";
  let n = ref 0 in
  let save t ~title =
    incr n;
    let fname = sprintf "graph%d.dat" !n in
    Curve_sampling.to_file t fname;
    fprintf fh "set output \"graph%d.png\"\n\
                plot %S with l lt 1 lw 2 title %S\n" !n fname title;
    fprintf fh "set output \"graph%d_p.png\"\n\
                plot %S with l lt 5 lw 2 title %S, \
                %S with p lt 1 pt 5 ps 0.2 title \"points\"\n"
      !n fname title fname
  in
  (*
  let f t x dx theta dtheta = 0.3 *. (sin t) +.
                              (0. *. (x +. dx +. theta +. dtheta)) in
  let u t = (Pendulum.solve 0.5 0.2 0.1 0.3 0.006 f) 0. 3.2 t in
  let x t = (u t).(0) and theta t = (u t).(1) in
  let dx t = (u t).(2) and dtheta t = (u t).(3) in
  let graph_x = fn x 0. 20. ~n:1000 in
  let graph_dx = fn dx 0. 20. ~n:1000 in
  let graph_theta_1 = fn theta 0. 20. ~n:1000 in
  let graph_theta_2 = fn theta 0.5 2.5 ~n:1000 in
  let graph_dtheta_1 = fn dtheta 0. 20. ~n:1000 in
  let graph_dtheta_2 = fn dtheta 0.5 2.5 ~n:1000 in
  save graph_x ~title:"x(t)";
  save graph_dx ~title:"d x(t)";
  save graph_theta_1 ~title:"theta(t)";
  save graph_theta_2 ~title:"theta(t)";
  save graph_dtheta_1 ~title:"d theta(t)";
  save graph_dtheta_2 ~title:"d theta(t)";
  *)
  (*
  let f omega t x dx theta dtheta = 0.3 *. (sin (omega *. t)) +.
                              (0. *. (x +. dx +. theta +. dtheta)) in
  let u omega t =
        (Pendulum.solve 0.5 0.2 0.1 0.3 0.006 (f omega)) 0. 3.2 t in
  let theta omega t = (u omega t).(1) and dtheta omega t = (u omega t).(3) in
  let graph_theta_1 = fn (theta 1.) 0. 10. ~n:1000 in
  let graph_theta_2 = fn (theta 2.) 0. 10. ~n:1000 in
  let graph_theta_3 = fn (theta 3.) 0. 10. ~n:1000 in
  let graph_theta_4 = fn (theta 4.) 0. 10. ~n:1000 in
  let graph_theta_5 = fn (theta 5.) 0. 10. ~n:1000 in
  let graph_theta_6 = fn (theta 6.) 0. 10. ~n:1000 in
  let graph_theta_7 = fn (theta 7.) 0. 10. ~n:1000 in
  let graph_theta_8 = fn (theta 8.) 0. 10. ~n:1000 in
  let graph_theta_9 = fn (theta 9.) 0. 10. ~n:1000 in
  let graph_theta_10 = fn (theta 10.) 0. 10. ~n:1000 in
  let graph_dtheta_1 = fn (dtheta 1.) 0. 10. ~n:1000 in
  let graph_dtheta_2 = fn (dtheta 2.) 0. 10. ~n:1000 in
  let graph_dtheta_3 = fn (dtheta 3.) 0. 10. ~n:1000 in
  let graph_dtheta_4 = fn (dtheta 4.) 0. 10. ~n:1000 in
  let graph_dtheta_5 = fn (dtheta 5.) 0. 10. ~n:1000 in
  let graph_dtheta_6 = fn (dtheta 6.) 0. 10. ~n:1000 in
  let graph_dtheta_7 = fn (dtheta 7.) 0. 10. ~n:1000 in
  let graph_dtheta_8 = fn (dtheta 8.) 0. 10. ~n:1000 in
  let graph_dtheta_9 = fn (dtheta 9.) 0. 10. ~n:1000 in
  let graph_dtheta_10 = fn (dtheta 10.) 0. 10. ~n:1000 in
  save graph_theta_1 ~title:"theta(t), omega = 1";
  save graph_theta_2 ~title:"theta(t), omega = 2";
  save graph_theta_3 ~title:"theta(t), omega = 3";
  save graph_theta_4 ~title:"theta(t), omega = 4";
  save graph_theta_5 ~title:"theta(t), omega = 5";
  save graph_theta_6 ~title:"theta(t), omega = 6";
  save graph_theta_7 ~title:"theta(t), omega = 7";
  save graph_theta_8 ~title:"theta(t), omega = 8";
  save graph_theta_9 ~title:"theta(t), omega = 9";
  save graph_theta_10 ~title:"theta(t), omega = 10";
  save graph_dtheta_1 ~title:"d theta(t), omega = 1";
  save graph_dtheta_2 ~title:"d theta(t), omega = 2";
  save graph_dtheta_3 ~title:"d theta(t), omega = 3";
  save graph_dtheta_4 ~title:"d theta(t), omega = 4";
  save graph_dtheta_5 ~title:"d theta(t), omega = 5";
  save graph_dtheta_6 ~title:"d theta(t), omega = 6";
  save graph_dtheta_7 ~title:"d theta(t), omega = 7";
  save graph_dtheta_8 ~title:"d theta(t), omega = 8";
  save graph_dtheta_9 ~title:"d theta(t), omega = 9";
  save graph_dtheta_10 ~title:"d theta(t), omega = 10";
  *)
  (*
  let solve_max segway_mass stick_mass speed length inertia f =
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
      let vec = Odepack.vec(Odepack.lsodar ode init 0. t ~mxstep:3000) in
      [| vec.{1}; vec.{2}; vec.{3}; vec.{4} |] in
    u in
  *)
  let f omega t x dx theta dtheta = 0.3 *. (sin (omega *. t)) +.
                              (0. *. (x +. dx +. theta +. dtheta)) in
  let u omega = Tours.root_solve 0.5 0.2 0.1 0.3 0.006 (f omega) in
  let graph_u = fn u 0. 5. ~n:100000 in
  save graph_u ~title:"Test";