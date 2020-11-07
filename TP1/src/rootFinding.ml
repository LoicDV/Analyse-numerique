(* Script in OCaml created by
 * Dupont Loïc, Marcelis Paolo and Vanhaverbeke Maximilien *)

let rec rootDeriv df a b =
  (*
   * Inputs :
   * - df : a function with an unique root in the interval ]a, b[,
   *        where if m is the root, the function is strictly positive on
   *        ]a, m[ and strictly negative on ]m, b[
   * - a : a float
   * - b : a float strictly greater than a
   *
   * Output :
   * - The set containing the unique solution of the function df
   *   in the interval ]a, b[
   *)
  let dfa = df a and dfb = df b and prec = 1e-10 in
    if dfa > 0. && dfb < 0. then
      Root1D.brent ~tol:prec df a b
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
  (*
   * Inputs :
   * - f : a function of class C2, unimodal, defined on the interval [a, b]
   * - df : a function, the derivative of f, with only simple zeros
   *        (i.e. the derivative of df must not equal 0. when df equals 0.
   * - a : a float
   * - b : a float strictly greater than a
   *
   * Output :
   * - The set of solutions of the function f in the interval [a, b]
   *)
  let fa = f a and fb = f b in
    if fa > 0. && fb > 0. then
      []
   else if fa = 0. && fb > 0. then
      [a]
   else if fa > 0. && fb = 0. then
      [b]
   else
      let m = rootDeriv df a b and prec = 1e-10 in
        if fa > 0. && fb <= 0. then
          let x = Root1D.brent ~tol:prec f m b in
            [x]
        else if fa <= 0. && fb > 0. then
          let x = Root1D.brent ~tol:prec f a m in
            [x]
        else
          let fm = f m in
            if fm > 0. then
              let x = Root1D.brent ~tol:prec f a m and
                  y = Root1D.brent ~tol:prec f m b in
                [x; y]
            else if fm = 0. then
              [m]
            else
              [];;