(* import rootFinding *)
(* import Root1d (je sais qu'il y est déjà mais plus simple à utiliser) *)

let q5 a b c =
(* (a,b,c) != (0,0,0) donc soit a ou b ou c est différent de 0
 * exmple : (0, 0, 1)
 *          (1, 1, 1) *)

    if a = 0. then
        match (b, c) with
            (0., c) -> print_string "no roots"            (*(0,0,c)   Cas 1*)
           |(b, 0.) -> print_float 0.;                    (*(0,b,0)   Cas 2*)
           |(b, c)  -> let x = -.c /. b in print_float x; (*(0,b,c)        *)

    if b = 0. then
        match (a, c) with
            (0., c) -> print_string "no roots"            (*(0,0,c)   Cas 1*)
           |(a, 0.) -> print_float 0.;                    (*(a,0,0)   Cas 3*)
           |(a, c)  ->                                    (*(a,0,c)        *)
            if (a < 0. && c > 0.) || (a > 0. && c < 0.) then
                let x1 = sqrt (-.c /. a) and x2 = -.sqrt (-.c /. a) in
                    print_float x1;
                    print_string"\n";
                    print_float x2;

    if c = 0. then
        match (a, b) with
            (0., b) -> print_float 0.;                    (*(0,b,0)   Cas 2*)
           |(a, 0.) -> print_float 0.;                    (*(a,0,0)   Cas 3*)
           |(a, b) -> let x = -.b /. a in                 (*(a,b,0)        *)
                          print_float 0.;
                          print_float x;
(*Il y a des cas qui se répetent dans les 3 if ci dessus*)


    if a != 0. && b != 0. && c != 0. then
    (* -b/a = somme des racines , c/a = produit des racines ,
        si c/a>0 alors -b/a peut etre utiliser comme borne car somme
        de deux positifs >= max{des deux positifs} (de meme pour negatifs) *)
        if a > 0. then
            let aa = -.a and bb = -.b and cc = -.c in
                let f x = aa *. x ** 2. +. bb *. x +. cc and
                df x = 2. *. x *. aa +. bb in
                    x1, x2 = rootFinding.rootFinding f df -.10 10.
                    (* Il faut trouver l'intervalle *)
                    print_float x1;
                    print_string"\n";
                    print_float x2;
        else
            let f x = a *. x ** 2. +. b *. x +. c and
            df x = 2. *. x *. a +. b in
                x1, x2 = rootFinding.rootFinding f df -.10 10.
                print_float x1;
                print_string"\n";
                print_float x2;
    None;;
