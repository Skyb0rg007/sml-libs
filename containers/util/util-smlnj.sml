
structure Util =
struct

fun ptrEq (a: 'a, b: 'a) =
   (Unsafe.cast a : word) = Unsafe.cast b

end

(* vim: set tw=0 ts=3 sw=3: *)
