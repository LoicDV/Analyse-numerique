type vec = (float, Stdlib.Bigarray.float64_elt, Stdlib.Bigarray.fortran_layout)
Stdlib.Bigarray.Array1.t
type mat = (float, Stdlib.Bigarray.float64_elt, Stdlib.Bigarray.fortran_layout)
Stdlib.Bigarray.Array2.t
type jacobian =
  | Auto_full
  | Auto_band of int * int
  | Full of float -> vec -> mat -> unit
  | Band of int * int * float -> vec -> int -> mat -> unit

