let () =
  let f t x dx theta dtheta = 0.3 *. (sin t) +.
                              (0. *. (x +. dx +. theta +. dtheta)) in
  Array.iter (Printf.printf "%.8f\n") ((Pendulum.solve 0.5 0.2 0.1 0.3 0.006 f)
                                      (Float.of_string Sys.argv.(1))
                                      (Float.of_string Sys.argv.(2))
                                      (Float.of_string Sys.argv.(3)))