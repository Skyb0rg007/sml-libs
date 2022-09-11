
signature HASHABLE_TYPE =
sig

type t

val hash: t -> word
val equals: t * t -> bool

end

(* vim: set tw=0 ts=3 sw=3: *)
