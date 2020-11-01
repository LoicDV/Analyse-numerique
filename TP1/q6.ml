let rec root a y i op1 op2 op3 =
    let arc = acos ((-.1.) /. a) in
        if op2 (2.0 *. i *. (op1 Float.pi arc)) 0. then
	    []
	else
	    let f x = x +. (a *. (sin x)) -. y in
	        let df x = 1. +. (a *. (cos x)) in
		    List.append (RootFinding.rootFinding f df (op1 (2. *. i *. Float.pi) arc)
		                             (2. *. (op3 i 1.) *. (op1 Float.pi arc)))
			   (root a y (op3 i 1.) op1 op2 op3);;

let q6 a y =
    if a >= (-.1.) && a <= 1. then
        root a y 0. (+.) (>) (+.)
    else if a >= 1. then
        root a y 0. (+.) (>) (+.)
    else
        root a y 0. (+.) (>) (+.);;

let display liste =
    List.iter (printf "%.8f\n") liste;;

let () =
    let array = Sys.argv in
        let a = float_of_string array.(1) and
            y = float_of_string array.(2) in
	        let liste = q6 a y in
		    display liste;;