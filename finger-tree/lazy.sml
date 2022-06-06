
structure Lazy :> LAZY =
struct

datatype 'a node =
   Strict of 'a
 | Lazy of (unit -> 'a)

type 'a t = 'a node ref

fun force (ref (Strict x)) = x
  | force (r as ref (Lazy th)) =
   let
      val x = th ()
   in
      r := Strict x
      ; x
   end

fun delay th = ref (Lazy th)

fun eager x = ref (Strict x)

fun fromSusp s = delay (fn () => SMLofNJ.Susp.force s)

fun map f r = delay (fn () => f (force r))

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
