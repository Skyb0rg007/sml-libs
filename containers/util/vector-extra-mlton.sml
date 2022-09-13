
structure VectorEx: VECTOR_EXTRA =
struct

open Vector

val create = MLton.Vector.create

val unfoldi = MLton.Vector.unfoldi

end

(* vim: set tw=0 ts=3 sw=3: *)
