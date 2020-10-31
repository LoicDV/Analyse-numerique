open Sys;;
open Printf

let display liste =
    List.iter (printf "%.8f\n") liste;;

let change_sign z =
    (-.z);;

let q5 a b c =
    if a = 0. then
        match (b, c) with
            (0., c) -> [];
           |(b, 0.) -> [0.];
           |(b, c)  -> let x = -.c /. b in [x];

    else
        if a > 0. then
            let a = change_sign a
            let b = change_sign b
            let c = change_sign c;;

        let f x = a *. x ** 2. +. b *. x +. c and
            df x = 2. *. x *. a +. b in
            let borne_d = max (4. *. abs_float(b) /. abs_float(a)),
                            2. *. sqrt(abs_float(c) /. abs_float(a)) in
                let borne_g = (-.borne_d) in
                (* 2. *. -.b/.(2.*.a) -. borne_d *)
                    liste = RootFinding.rootFinding f df borne_g borne_d
                    liste;;

let () =
    let a = Sys.argv 2;
    let b = Sys.argv 3;
    let c = Sys.argv 4;
    let liste = q5 a b c
    display liste;;