
functor NodeFn'(M: MONOID): NODE =
struct

infix <+>
val op <+> = M.+

structure M = M

datatype 'a t =
   Node2 of M.t * 'a * 'a
 | Node3 of M.t * 'a * 'a * 'a

fun node2 f (a, b) = Node2 (f a <+> f b, a, b)

fun node3 f (a, b, c) = Node3 (f a <+> f b <+> f c, a, b, c)

fun measure (Node2 (v, _, _)) = v
  | measure (Node3 (v, _, _, _)) = v

fun toDigit (Node2 (_, a, b)) = Digit.Two (a, b)
  | toDigit (Node3 (_, a, b, c)) = Digit.Three (a, b, c)

fun map meas f (Node2 (_, a, b)) = node2 meas (f a, f b)
  | map meas f (Node3 (_, a, b, c)) = node3 meas (f a, f b, f c)

fun mapWithPos meas1 meas2 f (vl, n) =
   case n of
      Node2 (_, a, b) =>
         let
            val vla = vl <+> meas1 a
         in
            node2 meas2 (f (vl, a), f (vla, b))
         end
    | Node3 (_, a, b, c) =>
         let
            val vla = vl <+> meas1 a
            val vlab = vla <+> meas1 b
         in
            node3 meas2 (f (vl, a), f (vla, b), f (vlab, c))
         end

fun search meas p (vl, n, vr) =
   case n of
      Node2 (_, a, b) =>
         let
            val va = vl <+> meas a
            val vb = meas b <+> vr
         in
            if p (va, vb)
               then (NONE, a, SOME (Digit.One b))
            else (SOME (Digit.One a), b, NONE)
         end
    | Node3 (_, a, b, c) =>
         let
            val va = vl <+> meas a
            val vab = va <+> meas b
            val vc = meas c <+> vr
            val vbc = meas b <+> vc
         in
            if p (va, vbc)
               then (NONE, a, SOME (Digit.Two (b, c)))
            else if p (vab, vc)
               then (SOME (Digit.One a), b, SOME (Digit.One c))
            else (SOME (Digit.Two (a, b)), c, NONE)
         end

fun split meas p (i, n) =
   case n of
      Node2 (_, a, b) =>
         let
            val va = i <+> meas a
         in
            if p va
               then (NONE, a, SOME (Digit.One b))
            else (SOME (Digit.One a), b, NONE)
         end
    | Node3 (_, a, b, c) =>
         let
            val va = i <+> meas a
            val vab = va <+> meas b
         in
            if p va
               then (NONE, a, SOME (Digit.Two (b, c)))
            else if p vab
               then (SOME (Digit.One a), b, SOME (Digit.One c))
            else (SOME (Digit.Two (a, b)), c, NONE)
         end

end
(* vim: set ft=sml tw=0 ts=3 sw=3: *)
