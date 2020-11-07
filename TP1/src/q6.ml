(* Script in OCaml created by
 * Dupont Loïc, Marcelis Paolo and Vanhaverbeke Maximilien *)

let rec root a y i arc f df acc op1 op2 =
    (*
     * Inputs :
     * - a : a float
     * - y : a float
     * - i : a float
     * - arc : the float acos ((-.1.) /. a) to avoid calulating it many times
     * - f : the function f x = x +. (a *. (sin x)) -. y
     * - df : the function df x = 1. +. (a *. (cos x))
     * - acc : a list, serves as an accumulator to implement terminal
                recursion
     * - op1 : a binary operator (+. or -.)
     * - op2 : a binary operator (+. or -.)
     *
     * Output :
     * - A part of the set of solutions of the equation
     *   x +. (a *. (sin x)) -. y = 0. with x the unknown
     *
     *   If op2 is (+.), the solutions retured are the ones greater than
     *   op1 (2. *. i *. Float.pi) arc
     *   Else if op2 is (-.), the solutions returned are the ones smaller than
     *   op1 (2. *. i *. Float.pi) arc
     *)
     let rootList = RootFinding.rootFinding f df
                    (op1 (2. *. i *. Float.pi) arc)
                    (op1 (2. *. (i +. 1.) *. Float.pi) arc) in
         if rootList = [] then
             acc
         else
             let newAcc = List.append acc rootList in
                 root a y (op2 i 1.) arc f df newAcc op1 op2;;

let q6 a y =
    (*
     * Inputs :
     * - a : a float
     * - y : a float
     *
     * Output :
     * - The set of solutions of the equation x + a sin(x) - y = 0
     *   x +. (a *. (sin x)) -. y = 0. with x the unknown
     *)
    let f x = x +. (a *. (sin x)) -. y in
        if a >= (-.1.) && a <= 1. then
            let prec = 0.00000001 in
                let r = Root1D.brent ~tol:prec f (y -. a) (y +. a) in
                    [r]
        else if a >= 1. then
            let df x = 1. +. (a *. (cos x)) and arc = acos ((-.1.) /. a) in
                let origin = Float.round ((y +. arc) /. (2. *. Float.pi)) in
                    List.append (root a y origin arc f df [] (-.) (+.))
                                (root a y (origin -. 1.) arc f df [] (-.) (-.))
        else
            let df x = 1. +. (a *. (cos x)) and arc = acos ((-.1.) /. a) in
                let origin = Float.round ((y -. arc) /. (2. *. Float.pi)) in
                    List.append (root a y origin arc f df [] (+.) (+.))
                                (root a y (origin -. 1.) arc f df [] (+.) (-.))
                    ;;

let display liste =
    List.iter (Printf.printf "%.8f\n") liste;;

let () =
    let array = Sys.argv in
        let a = Float.of_string array.(1) and
            y = Float.of_string array.(2) in
            let liste = q6 a y in
                display liste;;