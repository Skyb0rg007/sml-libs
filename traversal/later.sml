
structure Later: LATER =
struct

datatype 'a node =
   Strict of 'a
 | Lazy of (unit -> 'a)
 | Exn of exn

type 'a t = 'a node ref

exception Circular

fun pure x = ref (Strict x)

fun force r =
   case !r of
      Strict a => a
    | Lazy th =>
        (let
            val a = th ()
         in
            r := Strict a
            ; a
         end
         handle e => (r := Exn e; raise e))
    | Exn e => raise e

fun map f r = ref (Lazy (fn () => f (force r)))

fun map2 f (r1, r2) = ref (Lazy (fn () => f (force r1, force r2)))

fun ap (f, x) = ref (Lazy (fn () => force f (force x)))

fun ap2 (f, x, y) = ref (Lazy (fn () => force f (force x, force y)))

fun fix k =
   let
      val r = ref (Exn Circular)
      val res = k r
   in
      r := Strict res
      ; res
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
