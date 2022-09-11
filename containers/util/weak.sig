
signature WEAK_REFERENCE =
sig

type 'a t

val new: 'a -> 'a t
val get: 'a t -> 'a option

end

(* vim: set tw=0 ts=3 sw=3: *)
