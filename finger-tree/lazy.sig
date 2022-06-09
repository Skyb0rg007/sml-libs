
(* Like SMLofNJ.Susp, but without exception handling
 * We won't be raising any exceptions,
 * and it's slow to set up handlers
 *
 * However, that does mean that throwing an exception from
 * the `delay` thunk may cause unintuitive behavior
 *)
signature LAZY =
sig

type 'a t

(* Evaluate the reference, returning the result *)
val force: 'a t -> 'a

(* Create a delayed reference *)
val delay: (unit -> 'a) -> 'a t

(* Create an already-evaluated reference *)
val eager: 'a -> 'a t

(* Lazily map *)
val map: ('a -> 'b) -> 'a t -> 'b t

(* Return a value only if it is already evaluated *)
val nudge: 'a t -> 'a option

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
