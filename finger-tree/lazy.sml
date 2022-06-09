
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

fun map f r = delay (fn () => f (force r))

fun nudge (ref (Strict x)) = SOME x
  | nudge _ = NONE

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
