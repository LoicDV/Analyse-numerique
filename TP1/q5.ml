open Sys;;
open Printf

let display liste =
    List.iter (printf "%.8f\n") liste;;

let change_sign a b c =
    let a = -.a and b = -.b and c = -.c in
    a, b, c;;

let q5 a b c =
    if a = 0. then
        match (b, c) with
            (0., c) -> display []
           |(b, 0.) -> display [0.]
           |(b, c)  -> let x = -.c /. b in display [x];

    if a > 0. then
        let a, b, c = change_sign a b c;

    let f x = a *. x ** 2. +. b *. x +. c and
        df x = 2. *. x *. a +. b in
            liste = RootFinding.rootFinding f df _ _
            (* Il faut trouver l'intervalle *)
            liste
    None;;

let () =
    let a = Sys.argv 2;
    let b = Sys.argv 3;
    let c = Sys.argv 4;
    let liste = q5 a b c
    display liste;;