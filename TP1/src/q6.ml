(* Script in OCaml created by
 * Dupont Loïc, Marcelis Paolo and Vanhaverbeke Maximilien *)

let rec root a y i arc f df accum arcOp incrOp =
  (*
   * Inputs :
   * - a : a float
   * - y : a float
   * - i : a float
   * - arc : the float acos ((-.1.) /. a) to avoid calulating it many times
   * - f : the function f x = x +. (a *. (sin x)) -. y
   * - df : the function df x = 1. +. (a *. (cos x))
   * - accum : a float array, serves as an accumulator to implement terminal
               recursion
   * - arcOp : a binary operator (+. or -.), the sign of the arc term
   * - incrOp : a binary operator (+. or -.), controls whether i is incremented
   *            or decremented
   *
   * Output :
   * - A part of the set of solutions of the equation
   *   x +. (a *. (sin x)) -. y = 0. with x the unknown
   *
   *   If incrOp is (+.), the solutions retured are the ones greater than
   *   arcOp (2. *. i *. Float.pi) arc
   *   Else if incrOp is (-.), the solutions returned are the ones smaller than
   *   arcOp (2. *. i *. Float.pi) arc
   *)
  let rootList = RootFinding.rootFinding f df (arcOp (2. *. i *. Float.pi) arc)
                 (arcOp (2. *. (i +. 1.) *. Float.pi) arc) in
    if rootList = [||] then
      accum
    else
      let newAcc = Array.append accum rootList in
        root a y (incrOp i 1.) arc f df newAcc arcOp incrOp;;

let q6 a y =
  (*
   * Inputs :
   * - a : a float
   * - y : a float
   *
   * Output :
   * - The set of solutions of the equation x +. (a *. (sin x)) -. y = 0.
   *   with x the unknown
   *)
  let f x = x +. (a *. (sin x)) -. y in
    if a >= (-.1.) && a <= 1. then
      let r = Root1D.brent f (y -. a) (y +. a) in
        [|r|]
    else if a >= 1. then
      let df x = 1. +. (a *. (cos x)) and arc = acos ((-.1.) /. a) in
        let origin = Float.round ((y +. arc) /. (2. *. Float.pi)) in
          Array.append (root a y origin arc f df [||] (-.) (+.))
                      (root a y (origin -. 1.) arc f df [||] (-.) (-.))
    else
      let df x = 1. +. (a *. (cos x)) and arc = acos ((-.1.) /. a) in
        let origin = Float.round ((y -. arc) /. (2. *. Float.pi)) in
          Array.append (root a y origin arc f df [||] (+.) (+.))
                      (root a y (origin -. 1.) arc f df [||] (+.) (-.));;

let display liste =
  (*
   * Input :
   * - liste : a float array
   *
   * Output :
   * - Unit expression that prints each float in liste with a precision of 8
   *   decimals and go to the next line after each float printed
   *)
  Array.iter (Printf.printf "%.8f\n") liste;;

let () =
  (*
   * Main function, the inputs come from the command line
   *
   * Inputs :
   * - a : a float
   * - y : a float
   *
   * Output :
   * - Print on the standard output the set of solutions of the equation
   *   x +. (a *. (sin x)) -. y = 0. with x the unknown
   *)
  let array = Sys.argv in
    let a = Float.of_string array.(1) and
        y = Float.of_string array.(2) in
      let liste = q6 a y in
        display liste;;