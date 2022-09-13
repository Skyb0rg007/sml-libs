
structure Util =
struct

(* This *should* be safe(?) *)
fun ptrEq (a: 'a, b: 'a) =
   (Unsafe.cast a : Word.word) = Unsafe.cast b

end

(* vim: set tw=0 ts=3 sw=3: *)
