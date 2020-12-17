type vec = (float, Bigarray.float64_elt, Bigarray.fortran_layout)
Bigarray.Array1.t

val to_vec : float array -> vec
            (* Returns a object vector by taking the element of float array.
            *)

val solve : ?full:bool -> float -> float -> float -> float -> float ->
            (float -> float -> float -> float -> float -> float) ->
            float -> float -> float -> float array
            (*Return a fonction who takes 3 arguments, x_0, Θ_0 and
              t and this function returns a vector (x(t), Θ(t)) who is
              solution of the EDO :

              A ∂^2_t (x) = f(t, x, ∂_t x, Θ, ∂_t Θ)
                      (Θ)
            *)