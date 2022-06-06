
(* Like SMLofNJ.Susp, but without exception handling
 * We won't be raising any exceptions,
 * and it's slow to set up handlers *)
signature LAZY =
sig

type 'a t

val force: 'a t -> 'a
val delay: (unit -> 'a) -> 'a t
val eager: 'a -> 'a t
val fromSusp: 'a SMLofNJ.Susp.susp -> 'a t
val map: ('a -> 'b) -> 'a t -> 'b t

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
