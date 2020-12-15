type vec = (float, Bigarray.float64_elt, Bigarray.fortran_layout)
Bigarray.Array1.t

val to_vec : float array -> vec
            (* Returns a object vector by taking the element of float array.
            *)

val solve : float -> float -> float -> float -> float ->
            (float -> float -> float -> float -> float -> float) ->
            float -> float -> float -> float array
            (* ...

            *)