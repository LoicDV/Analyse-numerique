open Printf

let display liste =
    List.iter (printf "%.8f\n") liste;;

let q5_ a b c =
    let f x = a *. x ** 2. +. b *. x +. c and
        df x = 2. *. x *. a +. b in
        let borne_d = max ((4. *. abs_float(b)) /. abs_float(a))
        (2. *. sqrt(abs_float(c) /. abs_float(a))) in
            let borne_g = (-.borne_d) in
                let liste = RootFinding.rootFinding f df borne_g borne_d in
                    liste;;

let q5 a b c =
    if a = 0. then
        match (b, c) with
            (0., _) -> []
           |(_, 0.) -> [0.]
           |(b, c)  -> let x = -.c /. b in [x]
    else if a > 0. then
        q5_ (-.a) (-.b) (-.c)
    else
        q5_ a b c;;

let () =
    let array = Sys.argv in
        let a = float_of_string array.(1) and
        b = float_of_string array.(2) and
        c = float_of_string array.(3) in
            let liste = q5 a b c in
                display liste;;