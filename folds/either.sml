
structure Either :> EITHER =
struct

datatype ('a,'b) either = INL of 'a | INR of 'b

fun isLeft (INL _) = true
  | isLeft (INR _) = false

fun isRight (INL _) = false
  | isRight (INR _) = true

fun asLeft (INL x) = SOME x
  | asLeft (INR _) = NONE

fun asRight (INL _) = NONE
  | asRight (INR y) = SOME y

fun map (f, _) (INL x) = INL (f x)
  | map (_, g) (INR y) = INR (g y)

fun mapLeft f (INL x) = INL (f x)
  | mapLeft _ (INR y) = INR y

fun mapRight _ (INL x) = INL x
  | mapRight g (INR y) = INR (g y)

fun app (f, _) (INL x) = f x
  | app (_, g) (INR y) = g y

fun appLeft f (INL x) = f x
  | appLeft _ (INR _) = ()

fun appRight _ (INL _) = ()
  | appRight g (INR y) = g y

fun fold (f, _) z (INL x) = f (x, z)
  | fold (_, g) z (INR y) = g (y, z)

fun proj (INL x) = x
  | proj (INR y) = y

fun partition xs =
   List.foldr
     (fn (INL x, (ls, rs)) => (x :: ls, rs)
       | (INR y, (ls, rs)) => (ls, y :: rs))
     ([], [])
     xs

end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
