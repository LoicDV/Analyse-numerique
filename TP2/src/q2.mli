open Bigarray
type mat = (float, float64_elt, fortran_layout) Array2.t

val deriv : (float -> mat) -> (float -> mat) ->
    (float -> mat) -> (float -> mat) -> float -> mat
    (*  Returns an approximation of the derivative of the function x(t)
        in the form of a matrix.

        Our function takes 5 parameters in total. The first 4 are functions
        that send a real to precise matrices. In other words,
        t (a real) --> matrix, each of whose components will be evaluated in
        the value t. The matrices are not insignificant because they will be
        used to solve the linear equation A^t * A * X = A^t * b where the first
        and third function represent A and its derivative respectively as well
        as the second and fourth but for b.
    *)