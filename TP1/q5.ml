(* import rootFinding *)
(* import Root1d (je sais qu'il y est déjà mais plus simple à utiliser) *)

let q5 a b c =
(* (a,b,c) != (0,0,0) donc soit a ou b ou c est différent de 0
 * exmple : (0, 0, 1)
 *          (1, 1, 1) *)

    if a = 0. then
        match (b, c) with
            (0., c) -> print_string "no roots"
           |(b, 0.) -> print_float 0.;
           |(b, c)  -> let x = -.c /. b in print_float x;

    if b = 0. then
        match (a, c) with
            (0., c) -> print_string "no roots"
           |(a, 0.) -> print_float 0.;
           |(a, c)  -> if (a < 0. && c > 0.) || (a > 0. && c < 0.) then
                           let x1 = sqrt (-.c /. a) and
                           x2 = -.sqrt (-.c /. a) in
                               print_float x1;
                               print_newline;
                               print_float x2;

    if c = 0. then
        match (a, b) with
            (0., b) -> print_float 0.;
           |(a, 0.) -> print_float 0.;
           |(a, b) -> let x = -.b /. a in
                          print_float 0.;
                          print_float x;

    if a != 0. && b != 0. && c != 0. then
        if a > 0. then
            let aa = -.a and bb = -.b and cc = -.c in
                let f x = aa *. x ** 2. +. bb *. x +. cc and
                df x = 2. *. x *. aa +. bb in
                    x1, x2 = rootFinding.rootFinding f df _ _
                    (* Il faut trouver l'intervalle *)
                    print_float x1;
                    print_newline;
                    print_float x2;

    None;;