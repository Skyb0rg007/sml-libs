
signature HEAP =
sig

type t
type item

val new: unit -> t

val push: t * item -> unit
val peek: t -> item option
val pop: t -> item option

val size: t -> int
val isEmpty: t -> bool

end

(* vim: set tw=0 ts=3 sw=3: *)
