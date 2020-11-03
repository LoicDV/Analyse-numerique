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
        let arc = acos ((-.1.) /. a) in
            let origin = Float.round ((y +. arc) /. (2. *. Float.pi)) in
                List.append (root a y origin (-.) (+.))
                            (root a y (origin -. 1.) (-.) (-.))
    else
        let arc = acos ((-.1.) /. a) in
            let origin = Float.round ((y -. arc) /. (2. *. Float.pi)) in
                List.append (root a y origin (+.) (+.))
                            (root a y (origin -. 1.) (+.) (-.));;

let display liste =
    List.iter (Printf.printf "%.8f\n") liste;;

let () =
    let array = Sys.argv in
        let a = Float.of_string array.(1) and
            y = Float.of_string array.(2) in
	        let liste = q6 a y in
                display liste;;