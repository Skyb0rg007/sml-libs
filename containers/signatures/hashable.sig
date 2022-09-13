
signature HASHABLE_TYPE =
sig

include EQUATABLE_TYPE

val hash: t -> word

end

(* vim: set tw=0 ts=3 sw=3: *)
