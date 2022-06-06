
structure Digit : DIGIT =
struct

datatype 'a t =
   One of 'a
 | Two of 'a * 'a
 | Three of 'a * 'a * 'a
 | Four of 'a * 'a * 'a * 'a

fun cons (x, One a) = Two (x, a)
  | cons (x, Two (a, b)) = Three (x, a, b)
  | cons (x, Three (a, b, c)) = Four (x, a, b, c)
  | cons (_, Four _) = raise Fail "Digit.cons: could not cons onto Four"

fun snoc (One a, x) = Two (a, x)
  | snoc (Two (a, b), x) = Three (a, b, x)
  | snoc (Three (a, b, c), x) = Four (a, b, c, x)
  | snoc (Four _, _) = raise Fail "Digit.snoc: could not snoc onto Four"

fun head (One a) = a
  | head (Two (a, _)) = a
  | head (Three (a, _, _)) = a
  | head (Four (a, _, _, _)) = a

fun last (One a) = a
  | last (Two (_, b)) = b
  | last (Three (_, _, c)) = c
  | last (Four (_, _, _, d)) = d

fun tail (One _) = raise Fail "Digit.tail: could not take the tail of One"
  | tail (Two (_, b)) = One b
  | tail (Three (_, b, c)) = Two (b, c)
  | tail (Four (_, b, c, d)) = Three (b, c, d)

fun init (One _) = raise Fail "Digit.init: could not take the init of One"
  | init (Two (a, _)) = One a
  | init (Three (a, b, _)) = Two (a, b)
  | init (Four (a, b, c, _)) = Three (a, b, c)

fun map f (One a) = One (f a)
  | map f (Two (a, b)) = Two (f a, f b)
  | map f (Three (a, b, c)) = Three (f a, f b, f c)
  | map f (Four (a, b, c, d)) = Four (f a, f b, f c, f d)

fun reverse (One a) = One a
  | reverse (Two (a, b)) = Two (b, a)
  | reverse (Three (a, b, c)) = Three (c, b, a)
  | reverse (Four (a, b, c, d)) = Four (d, c, b, a)

fun mapWithPos op + f (p, d) =
   case d of
      One a => One (f (p, a))
    | Two (a, b) => Two (f (p, a), f (p + a, b))
    | Three (a, b, c) =>
         let
            val pa = p + a
         in
            Three (f (p, a), f (pa, b), f (pa + b, c))
         end
    | Four (a, b, c, d) =>
         let
            val pa = p + a
            val pab = pa + b
         in
            Four (f (p, a), f (pa, b), f (pab, c), f (pab + c, d))
         end

fun mapWithContext op + op * f (c1, d, c2) =
   case d of
      One a => One (f (c1, a, c2))
    | Two (a, b) => Two (f (c1, a, b * c2), f (c1 + a, b, c2))
    | Three (a, b, c) =>
         let
            val c1a = c1 + a
            val cc2 = c * c2
         in
            Three (f (c1, a, b * cc2), f (c1a, b, cc2), f (c1a + b, c, c2))
         end
    | Four (a, b, c, d) =>
         let
            val c1a = c1 + a
            val c1ab = c1a + b
            val dc2 = d * c2
            val cdc2 = c * dc2
         in
            Four (f (c1, a, b * cdc2),
                  f (c1a, b, cdc2),
                  f (c1ab, c, dc2),
                  f (c1ab + c, d, c2))
         end

fun foldlWithPos op + f acc (p, d) =
   case d of
      One a => f (p, a, acc)
    | Two (a, b) => f (p + a, b, f (p, a, acc))
    | Three (a, b, c) =>
         let
            val pa = p + a
         in
            f (pa + b, c, f (pa, b, f (p, a, acc)))
         end
    | Four (a, b, c, d) =>
         let
            val pa = p + a
            val pab = pa + b
         in
            f (pab + c, d, f (pab, c, f (pa, b, f (p, a, acc))))
         end

fun foldrWithPos op + f acc (p, d) =
   case d of
      One a => f (p, a, acc)
    | Two (a, b) => f (p, a, f (p + a, b, acc))
    | Three (a, b, c) =>
         let
            val pa = p + a
         in
            f (p, a, f (pa, b, f (pa + b, c, acc)))
         end
    | Four (a, b, c, d) =>
         let
            val pa = p + a
            val pab = pa + b
         in
            f (p, a, f (pa, b, f (pab, c, f (pab + c, d, acc))))
         end

fun foldlWithContext op + op * f acc (c1, d, c2) =
   case d of
      One a => f (c1, a, c2, acc)
    | Two (a, b) => f (c1 + a, b, c2, f (c1, a, b * c2, acc))
    | Three (a, b, c) =>
         let
            val c1a = c1 + a
            val cc2 = c * c2
         in
            f (c1a + b, c, c2,
               f (c1a, b, cc2,
                  f (c1, a, b * cc2, acc)))
         end
    | Four (a, b, c, d) =>
         let
            val c1a = c1 + a
            val c1ab = c1a + b
            val dc2 = d * c2
            val cdc2 = c * dc2
         in
            f (c1ab + c, d, c2,
               f (c1ab, c, dc2,
                  f (c1a, b, cdc2,
                     f (c1, a, b * cdc2, acc))))
         end

fun foldrWithContext op + op * f acc (c1, d, c2) =
   case d of
      One a => f (c1, a, c2, acc)
    | Two (a, b) => f (c1, a, b * c2, f (c1 + a, b, c2, acc))
    | Three (a, b, c) =>
         let
            val c1a = c1 + a
            val cc2 = c * c2
         in
            f (c1, a, b * cc2,
               f (c1a, b, cc2,
                  f (c1a + b, c, c2, acc)))
         end
    | Four (a, b, c, d) =>
         let
            val c1a = c1 + a
            val c1ab = c1a + b
            val dc2 = d * c2
            val cdc2 = c * dc2
         in
            f (c1, a, b * cdc2,
               f (c1a, b, cdc2,
                  f (c1ab, c, dc2,
                     f (c1ab + c, d, c2, acc))))
         end

fun foldMap op + f =
   fn One a => f a
    | Two (a, b) => f a + f b
    | Three (a, b, c) => f a + f b + f c
    | Four (a, b, c, d) => f a + f b + f c + f d

fun foldr op + acc =
   fn One a => a + acc
    | Two (a, b) => b + (a + acc)
    | Three (a, b, c) => c + (b + (a + acc))
    | Four (a, b, c, d) => d + (c + (b + (a + acc)))

fun foldl op + acc =
   fn One a => a + acc
    | Two (a, b) => a + (b + acc)
    | Three (a, b, c) => a + (b + (c + acc))
    | Four (a, b, c, d) => a + (b + (c + (d + acc)))

fun reduce f d = foldMap f (fn x => x) d

fun search op + op * pred (vl, d, vr) =
   case d of
      One a => (NONE, a, NONE)
    | Two (a, b) =>
         let
            val va = vl + a
            val vb = b * vr
         in
            if pred (va, vb)
               then (NONE, a, SOME (One b))
            else (SOME (One a), b, NONE)
         end
    | Three (a, b, c) =>
         let
            val va = vl + a
            val vab = va + b
            val vc = c * vr
            val vbc = b * vc
         in
            if pred (va, vbc)
               then (NONE, a, SOME (Two (b, c)))
            else if pred (vab, vc)
               then (SOME (One a), b, SOME (One c))
            else  (SOME (Two (a, b)), c, NONE)
         end
    | Four (a, b, c, d) =>
         let
            val va = vl + a
            val vab = va + b
            val vabc = vab + c
            val vd = d * vr
            val vcd = c * vd
            val vbcd = b * vcd
         in
            if pred (va, vbcd)
               then (NONE, a, SOME (Three (b, c, d)))
            else if pred (vab, vcd)
               then (SOME (One a), b, SOME (Two (c, d)))
            else if pred (vabc, vd)
               then (SOME (Two (a, b)), c, SOME (One d))
            else (SOME (Three (a, b, c)), d, NONE)
         end

fun split op + pred (p, d) =
   case d of
      One a => (NONE, a, NONE)
    | Two (a, b) =>
         let
            val pa = p + a
         in
            if pred pa
               then (NONE, a, SOME (One b))
            else (SOME (One a), b, NONE)
         end
    | Three (a, b, c) =>
         let
            val pa = p + a
            val pab = pa + b
         in
            if pred pa
               then (NONE, a, SOME (Two (b, c)))
            else if pred pab
               then (SOME (One a), b, SOME (One c))
            else (SOME (Two (a, b)), c, NONE)
         end
    | Four (a, b, c, d) =>
         let
            val pa = p + a
            val pab = pa + b
            val pabc = pab + c
         in
            if pred pa
               then (NONE, a, SOME (Three (b, c, d)))
            else if pred pab
               then (SOME (One a), b, SOME (Two (c, d)))
            else if pred pabc
               then (SOME (Two (a, b)), c, SOME (One d))
            else (SOME (Three (a, b, c)), d, NONE)
         end

fun toString tos (One a) =
   String.concat ["One ", tos a]
  | toString tos (Two (a, b)) =
   String.concat ["Two (", tos a, ", ", tos b, ")"]
  | toString tos (Three (a, b, c)) =
   String.concat ["Three (", tos a, ", ", tos b, ", ", tos c, ")"]
  | toString tos (Four (a, b, c, d)) =
   String.concat ["Four (", tos a, ", ", tos b, ", ", tos c, ", ", tos d, ")"]

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
