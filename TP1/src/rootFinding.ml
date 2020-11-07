(* Script in OCaml created by
 * Dupont Loïc, Marcelis Paolo and Vanhaverbeke Maximilien *)

let rec rootDeriv df a b =
    (* Under-Function of rootFinding
     * Find the roots of the derivative of f
     * that will be necessary to discuss
     * other possibility in the main function*)
    let dfa = df a and dfb = df b in
        if dfa > 0. && dfb < 0. then
            Root1D.brent ~tol:0.00000001 df a b
        else
            let mid = ((a +. b) /. 2.) in
                let dfm = df mid in
                    if dfm = 0. then
                        mid
                    else if dfm > 0. then
                        rootDeriv df mid b
                    else
                        rootDeriv df a mid;;

let rootFinding f df a b =
    (* Main function to discover roots of any unimodal fucntion
     * Use 4 arguments : the function with his derivative and
     * 2 float which are the interval to search the roots *)
    let fa = f a and fb = f b in
        if fa > 0. && fb > 0. then
            []
        else if fa = 0. && fb > 0. then
            [a]
        else if fa > 0. && fb = 0. then
            [b]
        else
            let m = rootDeriv df a b in
                if fa > 0. && fb <= 0. then
                    let x = Root1D.brent ~tol:0.00000001 f m b in
                        [x]
                else if fa <= 0. && fb > 0. then
                    let x = Root1D.brent ~tol:0.00000001 f a m in
                        [x]
                else
                    let fm = f m in
                        if fm > 0. then
                            let x = Root1D.brent ~tol:0.00000001 f a m and
                                y = Root1D.brent ~tol:0.00000001 f m b in
                                [x; y]
                        else if fm = 0. then
                            [m]
                        else
                            [];;