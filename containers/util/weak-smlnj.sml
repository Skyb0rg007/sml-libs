
structure Weak: WEAK_REFERENCE =
struct

type 'a t = 'a SMLofNJ.Weak.weak

val new = SMLofNJ.Weak.weak

val get = SMLofNJ.Weak.strong

end

(* vim: set tw=0 ts=3 sw=3: *)
