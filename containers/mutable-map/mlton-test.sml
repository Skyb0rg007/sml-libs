
fun go 0 = ()
  | go n = (Rand.rand (); go (n - 1))

val () = go 99

val () = print (Word.fmt StringCvt.DEC (Rand.rand ()) ^ "\n")

(* vim: set tw=0 ts=3 sw=3: *)
