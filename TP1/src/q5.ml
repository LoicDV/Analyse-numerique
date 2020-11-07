(* Script in OCaml created by
 * Dupont Loïc, Marcelis Paolo and Vanhaverbeke Maximilien *)

open Printf

(* WARNING : This script is to discover roots of function ax^2 + bx + c where
 * this function is unimodal ! (that's why we une rootFinding) *)

let display liste =
    (* Function to display all elements in the argument of type List
     * the print will make float with 8 digits and one per line *)
    List.iter (printf "%.8f\n") liste;;

let q5_ a b c =
    (* Under-function which calculate the roots of function which is
     * ax^2 + bx + c = 0 and also use rootFinding in rootFinding.ml *)
    let f x = a *. x ** 2. +. b *. x +. c and
        df x = 2. *. x *. a +. b in
        let borne_d = max ((4. *. abs_float(b)) /. abs_float(a))
        (2. *. sqrt(abs_float(c) /. abs_float(a))) in
            let borne_g = (-.borne_d) in
                let liste = RootFinding.rootFinding f df borne_g borne_d in
                    liste;;

let q5 a b c =
    (* Takes 3 arguments (float) and resolve ax^2 + bx + c = 0
     * This function use another function call q5_ a b c which
     * all mathematics are in q5_ (this is for seperate the function) *)
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
    (* Main function
     * Takes arguments from command line, transform them into float
     * and use these floats for q5 *)
    let array = Sys.argv in
        let a = float_of_string array.(1) and
        b = float_of_string array.(2) and
        c = float_of_string array.(3) in
            let liste = q5 a b c in
                display liste;;