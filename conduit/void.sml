
structure Void =
struct

datatype t = T of t

fun absurd (T x) = absurd x

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
