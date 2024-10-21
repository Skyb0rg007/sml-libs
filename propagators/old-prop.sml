
structure Prop: PROP =
struct

datatype 'a t =
   Nullary of (unit -> 'a Cell.t)
 | Unary of ('a Cell.t * 'a Cell.t -> unit) * 'a t
 | Binary of ('a Cell.t * 'a Cell.t * 'a Cell.t -> unit) * 'a t * 'a t

datatype 'a deref =
   DerefNullary of unit -> 'a Cell.t
 | DerefUnary of ('a Cell.t * 'a Cell.t -> unit) * 'a
 | DerefBinary of ('a Cell.t * 'a Cell.t * 'a Cell.t -> unit) * 'a * 'a

fun mapDeref _ (DerefNullary n) = DerefNullary n
  | mapDeref f (DerefUnary (k, a)) = DerefUnary (k, f a)
  | mapDeref f (DerefBinary (k, a, b)) = DerefBinary (k, f a, f b)

fun a + b =
   let
      fun prop (x, y, z) =
         (Cell.lift2 Int.+ (x, y, z)
          ; Cell.lift2 Int.- (z, x, y)
          ; Cell.lift2 Int.- (z, y, x))
   in
      Binary (prop, a, b)
   end

fun a * b =
   let
      fun prop (x, y, z) =
         (Cell.lift2 Int.* (x, y, z)
          ; Cell.watch z
             (fn 0 =>
                 (Cell.watch x (fn 0 => () | _ => Cell.write (y, 0))
                  ; Cell.watch y (fn 0 => () | _ => Cell.write (x, 0)))
               | c =>
                 (Cell.watch x (fn a => Cell.write (y, c div a))
                  ; Cell.watch y (fn b => Cell.write (x, c div b)))))
   in
      Binary (prop, a, b)
   end

fun abs a =
   let
      fun prop (x, y) =
         (Cell.lift1 Int.abs (x, y)
          ; Cell.watch y (fn 0 => Cell.write (x, 0) | _ => ()))
   in
      Unary (prop, a)
   end

fun sign a =
   let
      fun prop (x, y) =
         (Cell.lift1 Int.sign (x, y)
          ; Cell.watch y (fn 0 => Cell.write (x, 0) | _ => ()))
   in
      Unary (prop, a)
   end

fun mergeInt (a, b) =
   if a = b
      then NoChange
   else Contradiction (Int.toString a ^ " != " ^ Int.toString b)

fun fromInt n = Nullary (fn () => Cell.known mergeInt n)

fun arg c = Nullary (fn () => c)

end

(* vim: set tw=0 ts=3 sw=3: *)
