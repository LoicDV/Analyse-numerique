open Curve_sampling
open Printf

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

  let f t x dx theta dtheta = 0.3 *. (sin t) +.
                              (0. *. (x +. dx +. theta +. dtheta)) in
  let u t = (Pendulum.solve 0.5 0.2 0.1 0.3 0.006 f) 0. 3.2 t in
  let x t = (u t).(0) and theta t = (u t).(1) in
  let dx t = (u t).(2) and dtheta t = (u t).(3) in
  let graph_x = fn x 0. 20. ~n:1000 in
  let graph_theta = fn theta 0. 20. ~n:1000 in
  let graph_dx = fn dx 0. 20. ~n:1000 in
  let graph_dtheta = fn dtheta 0. 20. ~n:1000 in
  save graph_x ~title:"x(t)";
  save graph_theta ~title:"theta(t)";
  save graph_dx ~title:"d x(t)";
  save graph_dtheta ~title:"d theta(t)";