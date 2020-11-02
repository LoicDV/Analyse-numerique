let rec root a y i op1 op2 = 
    let arc = acos ((-.1.) /. a) in
        let f x = x +. (a *. (sin x)) -. y in
            let df x = 1. +. (a *. (cos x)) in
                let list = RootFinding.rootFinding f df
                           (op1 (2. *. i *. Float.pi) arc)
		           (op1 (2. *. (i +. 1.) *. Float.pi) arc) in
                    if list = [] then
                        []
                    else
                        List.append list (root a y (op2 i 1.) op1 op2);;

let q6 a y =
    if a >= (-.1.) && a <= 1. then
	let f x = x +. (a *. (sin x)) -. y in
            let r = Root1D.brent f (y -. 1.) (y +. 1.) in
                [r]
    else if a >= 1. then
        List.append (root a y 0. (-.) (+.))
                    (root a y (-.1.) (-.) (-.))
    else
        List.append (root a y 0. (+.) (+.))
                    (root a y (-.1.) (+.) (-.));;

let display liste =
    List.iter (Printf.printf "%.8f\n") liste;;

let () =
    let a = read_float () in
        let y = read_float () in
            display (q6 a y)

(* Il reste à réussir à mettre les float dans la commande *)
(*
let () =
    let array = Sys.argv in
        let a = Float.of_string array.(1) and
            y = Float.of_string array.(2) in
		Printf.printf a;;
*)