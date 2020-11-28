val deriv : ('a -> 'a array array) -> ('a -> 'a array array) ->
    ('a -> 'a array array) -> ('a -> 'a array array) -> float -> 'a array array
(** Returns an approximation of the derivative of the function x(t)
    in the form of a matrix.

    The function thus takes 4 parameters which are functions that
    send a real t in a matrix and also as another parameter, the t in question.
*)