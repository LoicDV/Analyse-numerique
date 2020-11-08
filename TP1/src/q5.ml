(* Script in OCaml created by
 * Dupont Loïc, Marcelis Paolo and Vanhaverbeke Maximilien *)

let unimodalQ5 a b c =
  (*
   * Inputs :
   * - a : a float strictly smaller than 0.
   * - b : a float
   * - c : a float
   *
   * Output :
   * - The set of solutions of the equation
   *   (a *. (x ** 2.)) +. (b *. x) +. c = 0. with x the unknown
   *)
  let f x = (a *. (x ** 2.)) +. (b *. x) +. c and
    df x = (2. *. a *. x) +. b in
    let right = max ((4. *. (abs_float b)) /. (abs_float a))
                     (2. *. (sqrt ((abs_float c) /. (abs_float a)))) in
      let left = (-.right) in
        let liste = RootFinding.rootFinding f df left right in
          liste;;

let q5 a b c =
  (*
   * Inputs :
   * - a : a float
   * - b : a float
   * - c : a float
   *
   * Output :
   * - The set of solutions of the equation
   *   (a *. (x ** 2.)) +. (b *. x) +. c = 0. with x the unknown
   *)
  if a = 0. then
    match (b, c) with
       (0., _) -> [||]
      |(_, 0.) -> [|0. |]
      |(b, c)  -> let x = -.c /. b in [|x|]
  else if a > 0. then
    (* We seek the roots of the opposite function as it is unimodal and has
     * the same roots *)
    unimodalQ5 (-.a) (-.b) (-.c)
  else
    unimodalQ5 a b c;;

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
   * - b : a float
   * - c : a float
   *
   * Output :
   * - Print on the standard output the set of solutions of the equation
   *   (a *. (x ** 2.)) +. (b *. x) +. c = 0. with x the unknown
   *)
  let array = Sys.argv in
    let a = float_of_string array.(1) and
        b = float_of_string array.(2) and
        c = float_of_string array.(3) in
      let liste = q5 a b c in
        display liste;;