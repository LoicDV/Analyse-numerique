let () =
  let f t x dx theta dtheta = 0.3 *. (sin t) +.
                              (0. *. (x +. dx +. theta +. dtheta)) in
  let u t = (Pendulum.solve ~full:true 0.5 0.2 0.1 0.3 0.006 f) 0. 3.2 t in
  let theta t = (u t).(1) and dtheta t = (u t).(3) in
  Printf.printf "%.8f\n" (theta (Root1D.brent ~tol:1e-9 dtheta 0.5 2.5));