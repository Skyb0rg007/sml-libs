
structure Either: EITHER =
struct

datatype ('a, 'b) either = INL of 'a | INR of 'b

fun isLeft (INL _) = true
  | isLeft (INR _) = false

fun isRight (INL _) = false
  | isRight (INR _) = true

fun asLeft (INL a) = SOME a
  | asLeft (INR _) = NONE

fun asRight (INL _) = NONE
  | asRight (INR b) = SOME b

fun map (f, _) (INL a) = INL (f a)
  | map (_, g) (INR b) = INR (g b)

fun app f _ (INL a) = f a
  | app _ g (INR b) = g b

fun fold f _ z (INL a) = f (a, z)
  | fold _ g z (INR b) = g (b, z)

fun proj (INL a) = a
  | proj (INR a) = a

fun partition xs =
   List.foldr
   (fn (INL a, (l, r)) => (a :: l, r)
     | (INR b, (l, r)) => (l, b :: r))
   ([], [])
   xs

fun mapLeft f (INL a) = INL (f a)
  | mapLeft _ (INR b) = INR b

fun mapRight _ (INL a) = INL a
  | mapRight g (INR b) = INR (g b)

fun appLeft f (INL a) = f a
  | appLeft _ (INR _) = ()

fun appRight _ (INL _) = ()
  | appRight g (INR b) = g b

end

(* vim: set tw=0 ts=3 sw=3: *)
