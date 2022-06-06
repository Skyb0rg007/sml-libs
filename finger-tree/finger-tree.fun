
functor FingerTree(S: FINGER_TREE_STRUCTS) :> FINGER_TREE =
struct

open S

infix <+>
val op <+> = Measure.+

datatype 'a digit =
   One of 'a
 | Two of 'a * 'a
 | Three of 'a * 'a * 'a
 | Four of 'a * 'a * 'a * 'a

datatype 'a node =
   Node2 of Measure.t * 'a * 'a
 | Node3 of Measure.t * 'a * 'a * 'a

datatype 'a elem =
   Item of 'a Item.t
 | Node of 'a elem node

datatype 'a t =
   Empty
 | Single of 'a elem
 | Deep of Measure.t Lazy.t * 'a elem digit * 'a t Lazy.t * 'a elem digit

fun measureNode (Node2 (v, _, _)) = v
  | measureNode (Node3 (v, _, _, _)) = v

fun measureElem (Item x) = Item.measure x
  | measureElem (Node n) = measureNode n

fun measureDigit (One a) = measureElem a
  | measureDigit (Two (a, b)) = measureElem a <+> measureElem b
  | measureDigit (Three (a, b, c)) = measureElem a <+> measureElem b <+> measureElem c
  | measureDigit (Four (a, b, c, d)) = measureElem a <+> measureElem b <+> measureElem c <+> measureElem d

fun measure Empty = Measure.zero
  | measure (Single a) = measureElem a
  | measure (Deep (v, _, _, _)) = Lazy.force v

(* Conversions and smart constructors *)

fun node2 (a, b) = Node2 (measureElem a <+> measureElem b, a, b)

fun node3 (a, b, c) = Node3 (measureElem a <+> measureElem b <+> measureElem c, a, b, c)

fun node2' (a, b) = Node (node2 (a, b))

fun node3' (a, b, c) = Node (node3 (a, b, c))

fun deep (pr, m, sf) =
   let
      fun v () = measureDigit pr <+> measure (Lazy.force m) <+> measureDigit sf
   in
      Deep (Lazy.delay v, pr, m, sf)
   end

fun nodeToDigit (Node2 (_, a, b)) = Two (a, b)
  | nodeToDigit (Node3 (_, a, b, c)) = Three (a, b, c)

fun digitToTree (One a) = Single a
  | digitToTree (Two (a, b)) = deep (One a, Lazy.eager Empty, One b)
  | digitToTree (Three (a, b, c)) = deep (Two (a, b), Lazy.eager Empty, One c)
  | digitToTree (Four (a, b, c, d)) = deep (Two (a, b), Lazy.eager Empty, Two (c, d))

val _: 'a node -> 'a digit = nodeToDigit
val _: 'a elem digit -> 'a t = digitToTree

(* Mapping functions *)

local
   fun mapDigit f (One a) = One (f a)
     | mapDigit f (Two (a, b)) = Two (f a, f b)
     | mapDigit f (Three (a, b, c)) = Three (f a, f b, f c)
     | mapDigit f (Four (a, b, c, d)) = Four (f a, f b, f c, f d)

   fun mapNode f (Node2 (_, a, b)) = node2 (f a, f b)
     | mapNode f (Node3 (_, a, b, c)) = node3 (f a, f b, f c)

   fun mapElem f (Item x) = Item (f x)
     | mapElem f (Node n) = Node (mapNode (mapElem f) n)

   fun mapTree _ Empty = Empty
     | mapTree f (Single x) = Single (f x)
     | mapTree f (Deep (_, pr, m, sf)) =
      deep (mapDigit f pr, Lazy.map (mapTree f) m, mapDigit f sf)
in
   fun map f = mapTree (mapElem f)
end

local
   fun mapWPDigit f (vl, One a) = One (f (vl, a))
     | mapWPDigit f (vl, Two (a, b)) =
      let
         val vla = vl <+> measureElem a
      in
         Two (f (vl, a), f (vla, b))
      end
     | mapWPDigit f (vl, Three (a, b, c)) =
      let
         val vla = vl <+> measureElem a
         val vlab = vla <+> measureElem b
      in
         Three (f (vl, a), f (vla, b), f (vlab, c))
      end
     | mapWPDigit f (vl, Four (a, b, c, d)) =
      let
         val vla = vl <+> measureElem a
         val vlab = vla <+> measureElem b
         val vlabc = vlab <+> measureElem c
      in
         Four (f (vl, a), f (vla, b), f (vlab, c), f (vlabc, d))
      end

   fun mapWPNode f (vl, Node2 (_, a, b)) =
      let
         val vla = vl <+> measureElem a
      in
         node2 (f (vl, a), f (vla, b))
      end
     | mapWPNode f (vl, Node3 (_, a, b, c)) =
      let
         val vla = vl <+> measureElem a
         val vlab = vla <+> measureElem b
      in
         node3 (f (vl, a), f (vla, b), f (vlab, c))
      end

   fun mapWPTree _ (_, Empty) = Empty
     | mapWPTree f (vl, Single x) = Single (f (vl, x))
     | mapWPTree f (vl, Deep (_, pr, m, sf)) =
      let
         val m = Lazy.force m
         val vlp = vl <+> measureDigit pr
         val vlpm = vlp <+> measure m
      in
         deep (mapWPDigit f (vl, pr),
               Lazy.eager (mapWPTree f (vlp, m)),
               mapWPDigit f (vlpm, sf))
      end

   fun mapWPElem f (vl, Item x) = Item (f (vl, x))
     | mapWPElem f (vl, Node n) = Node (mapWPNode (mapWPElem f) (vl, n))
in
   fun mapWithPos f t = mapWPTree (mapWPElem f) (Measure.zero, t)
end

local
   fun mapWCDigit f (vl, One a, vr) = One (f (vl, a, vr))
     | mapWCDigit f (vl, Two (a, b), vr) =
      let
         val vla = vl <+> measureElem a
         val vbr = measureElem b <+> vr
      in
         Two (f (vl, a, vbr), f (vla, b, vr))
      end
     | mapWCDigit f (vl, Three (a, b, c), vr) =
      let
         val vla = vl <+> measureElem a
         val vlab = vla <+> measureElem b
         val vcr = measureElem c <+> vr
         val vbcr = measureElem b <+> vcr
      in
         Three (f (vl, a, vbcr), f (vla, b, vcr), f (vlab, c, vr))
      end
     | mapWCDigit f (vl, Four (a, b, c, d), vr) =
      let
         val vla = vl <+> measureElem a
         val vlab = vla <+> measureElem b
         val vlabc = vlab <+> measureElem c
         val vdr = measureElem d <+> vr
         val vcdr = measureElem c <+> vdr
         val vbcdr = measureElem b <+> vcdr
      in
         Four (f (vl, a, vbcdr), f (vla, b, vcdr), f (vlab, c, vdr), f (vlabc, d, vr))
      end

   fun mapWCTree _ (_, Empty, _) = Empty
     | mapWCTree f (vl, Single x, vr) = Single (f (vl, x, vr))
     | mapWCTree f (vl, Deep (_, pr, m, sf), vr) =
      let
         val m = Lazy.force m
         val vm = measure m
         val vlp = vl <+> measureDigit pr
         val vlpm = vlp <+> vm
         val vsr = measureDigit sf <+> vr
         val vmsr = vm <+> vsr
      in
         deep (mapWCDigit f (vl, pr, vmsr),
               Lazy.eager (mapWCTree f (vlp, m, vsr)),
               mapWCDigit f (vlpm, sf, vr))
      end

   fun mapWCNode f (vl, Node2 (_, a, b), vr) =
      let
         val vla = vl <+> measureElem a
         val vbr = measureElem b <+> vr
      in
         node2 (f (vl, a, vbr), f (vla, b, vr))
      end
     | mapWCNode f (vl, Node3 (_, a, b, c), vr) =
      let
         val vla = vl <+> measureElem a
         val vlab = vla <+> measureElem b
         val vcr = measureElem c <+> vr
         val vbcr = measureElem b <+> vcr
      in
         node3 (f (vl, a, vbcr), f (vla, b, vcr), f (vlab, c, vr))
      end

   fun mapWCElem f (vl, Item x, vr) = Item (f (vl, x, vr))
     | mapWCElem f (vl, Node n, vr) = Node (mapWCNode (mapWCElem f) (vl, n, vr))
in
   fun mapWithContext f t = mapWCTree (mapWCElem f) (Measure.zero, t, Measure.zero)
end

val _: ('a Item.t -> 'b Item.t) -> 'a t -> 'b t = map
val _: (Measure.t * 'a Item.t -> 'b Item.t) -> 'a t -> 'b t = mapWithPos
val _: (Measure.t * 'a Item.t * Measure.t -> 'b Item.t) -> 'a t -> 'b t = mapWithContext

(* Folding functions *)

local
in
   fun foldl _ = raise Fail "NYI"
end

local
   fun foldlWPDigit f z (vl, One a) = f (vl, a, z)
     | foldlWPDigit f z (vl, Two (a, b)) =
      let
         val vla = vl <+> measureElem a
      in
        f (vla, b, f (vl, a, z))
      end
     | foldlWPDigit f z (vl, Three (a, b, c)) =
      let
         val vla = vl <+> measureElem a
         val vlab = vla <+> measureElem b
      in
         f (vlab, c, f (vla, b, f (vl, a, z)))
      end
     | foldlWPDigit f z (vl, Four (a, b, c, d)) =
      let
         val vla = vl <+> measureElem a
         val vlab = vla <+> measureElem b
         val vlabc = vlab <+> measureElem c
      in
         f (vlabc, d, f (vlab, c, f (vla, b, f (vl, a, z))))
      end

   fun foldlWPNode f z (vl, Node2 (_, a, b)) =
      let
         val vla = vl <+> measureElem a
      in
         f (vla, b, f (vl, a, z))
      end
     | foldlWPNode f z (vl, Node3 (_, a, b, c)) =
      let
         val vla = vl <+> measureElem a
         val vlab = vla <+> measureElem b
      in
         f (vlab, c, f (vla, b, f (vl, a, z)))
      end

   fun foldlWPTree _ z (_, Empty) = z
     | foldlWPTree f z (vl, Single x) = f (vl, x, z)
     | foldlWPTree f z (vl, Deep (_, pr, m, sf)) =
      let
         val m = Lazy.force m
         val vlp = vl <+> measureDigit pr
         val vlpm = vlp <+> measure m
      in
         foldlWPDigit f (foldlWPTree f (foldlWPDigit f z (vl, pr)) (vlp, m)) (vlpm, sf)
      end

   fun liftElem f (v, Item x, z) = f (v, x, z)
     | liftElem f (v, Node n, z) = foldlWPNode (liftElem f) z (v, n)
in
   fun foldlWithPos f z t = foldlWPTree (liftElem f) z (Measure.zero, t)
end

local
in
   fun foldlWithContext _ = raise Fail "NYI"
end

local
in
   fun foldr _ = raise Fail "NYI"
end

local
in
   fun foldrWithPos _ = raise Fail "NYI"
end

local
in
   fun foldrWithContext _ = raise Fail "NYI"
end

fun toList t = foldr op :: [] t

(* Construction *)

val empty = Empty

fun singleton x = Single (Item x)

fun consDigit (a, One b) = Two (a, b)
  | consDigit (a, Two (b, c)) = Three (a, b, c)
  | consDigit (a, Three (b, c, d)) = Four (a, b, c, d)
  | consDigit (_, Four _) = raise Fail "consDigit: Four"

fun consTree (a, Empty) = Single a
  | consTree (a, Single b) = deep (One a, Lazy.eager Empty, One b)
  | consTree (a, Deep (v, Four (b, c, d, e), m, sf)) =
   Deep (Lazy.map (fn v => measureElem a <+> v) v,
         Two (a, b),
         Lazy.eager (consTree (node3' (c, d, e), Lazy.force m)),
         sf)
  | consTree (a, Deep (v, pr, m, sf)) =
   Deep (Lazy.map (fn v => measureElem a <+> v) v,
         consDigit (a, pr),
         m,
         sf)

fun cons (a, t) = consTree (Item a, t)

fun snocDigit (One a, b) = Two (a, b)
  | snocDigit (Two (a, b), c) = Three (a, b, c)
  | snocDigit (Three (a, b, c), d) = Four (a, b, c, d)
  | snocDigit (Four _, _) = raise Fail "snocDigit: Four"

fun snocTree (Empty, a) = Single a
  | snocTree (Single a, b) = deep (One a, Lazy.eager Empty, One b)
  | snocTree (Deep (v, pr, m, Four (a, b, c, d)), e) =
   Deep (Lazy.map (fn v => v <+> measureElem e) v,
         pr,
         Lazy.eager (snocTree (Lazy.force m, node3' (a, b, c))),
         Two (d, e))
  | snocTree (Deep (v, pr, m, sf), x) =
   Deep (Lazy.map (fn v => v <+> measureElem x) v,
         pr,
         m,
         snocDigit (sf, x))

fun snoc (t, a) = snocTree (t, Item a)

fun fromList xs = List.foldr cons empty xs

(* Query *)

fun isEmpty Empty = true
  | isEmpty _ = false

fun force t = ignore (measure t)

(* Destruction *)

fun headDigit (One a) = a
  | headDigit (Two (a, _)) = a
  | headDigit (Three (a, _, _)) = a
  | headDigit (Four (a, _, _, _)) = a

fun tailDigit (One _) = raise Fail "tailDigit: One"
  | tailDigit (Two (_, b)) = One b
  | tailDigit (Three (_, b, c)) = Two (b, c)
  | tailDigit (Four (_, b, c, d)) = Three (b, c, d)

fun rotL (m, sf) =
   case viewlTree m of
      NONE => digitToTree sf
    | SOME (Node a, m') =>
         Deep (Lazy.eager (measure m <+> measureDigit sf),
               nodeToDigit a,
               m',
               sf)
    | SOME (Item _, _) => raise Fail "rotL: Item"

and viewlTree Empty = NONE
  | viewlTree (Single x) = SOME (x, Lazy.eager Empty)
  | viewlTree (Deep (_, One x, m, sf)) = SOME (x, Lazy.map (fn m => rotL (m, sf)) m)
  | viewlTree (Deep (_, pr, m, sf)) = SOME (headDigit pr, Lazy.delay (fn () => deep (tailDigit pr, m, sf)))

fun viewl t =
   case viewlTree t of
      NONE => NONE
    | SOME (Item x, t') => SOME (x, t')
    | SOME (Node _, _) => raise Fail "viewl: Node"

fun lastDigit (One a) = a
  | lastDigit (Two (_, b)) = b
  | lastDigit (Three (_, _, c)) = c
  | lastDigit (Four (_, _, _, d)) = d

fun initDigit (One _) = raise Fail "tailDigit: One"
  | initDigit (Two (a, _)) = One a
  | initDigit (Three (a, b, _)) = Two (a, b)
  | initDigit (Four (a, b, c, _)) = Three (a, b, c)

fun rotR (pr, m) =
   case viewrTree m of
      NONE => digitToTree pr
    | SOME (m', Node a) =>
         Deep (Lazy.eager (measureDigit pr <+> measure m),
               pr,
               m',
               nodeToDigit a)
    | SOME (_, Item _) => raise Fail "rotR: Item"

and viewrTree Empty = NONE
  | viewrTree (Single x) = SOME (Lazy.eager Empty, x)
  | viewrTree (Deep (_, pr, m, One x)) = SOME (Lazy.map (fn m => rotR (pr, m)) m, x)
  | viewrTree (Deep (_, pr, m, sf)) = SOME (Lazy.delay (fn () => deep (pr, m, initDigit sf)), lastDigit sf)

fun viewr t =
   case viewrTree t of
      NONE => NONE
    | SOME (t', Item x) => SOME (t', x)
    | SOME (_, Node _) => raise Fail "viewr: Node"

(* Append *)

(* `addDigitsN (t1, d1, ..., d2, t2)`
 * Append the arguments in order
 * t1 and t2 are subtrees,
 * d1 and d2 are digits,
 * the ... contains N `Elem.t`s *)

(* `appTreeN (t1, ..., t2)`
 * Append the arguments in order
 * t1 and t2 are subtrees,
 * the ... contains N `Elem.t`s
 *)

fun addDigits1 (t1, d1, x, d2, t2) =
   case (d1, x, d2) of
      (One a, b, One c) => appTree1 (t1, node3' (a, b, c), t2)
    | (One a, b, Two (c, d)) => appTree2 (t1, node2' (a, b), node2' (c, d), t2)
    | (One a, b, Three (c, d, e)) => appTree2 (t1, node3' (a, b, c), node2' (d, e), t2)
    | (One a, b, Four (c, d, e, f)) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)

    | (Two (a, b), c, One d) => appTree2 (t1, node2' (a, b), node2' (c, d), t2)
    | (Two (a, b), c, Two (d, e)) => appTree2 (t1, node3' (a, b, c), node2' (d, e), t2)
    | (Two (a, b), c, Three (d, e, f)) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)
    | (Two (a, b), c, Four (d, e, f, g)) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)

    | (Three (a, b, c), d, One e) => appTree2 (t1, node3' (a, b, c), node2' (d, e), t2)
    | (Three (a, b, c), d, Two (e, f)) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)
    | (Three (a, b, c), d, Three (e, f, g)) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)
    | (Three (a, b, c), d, Four (e, f, g, h)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)

    | (Four (a, b, c, d), e, One f) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)
    | (Four (a, b, c, d), e, Two (f, g)) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)
    | (Four (a, b, c, d), e, Three (f, g, h)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)
    | (Four (a, b, c, d), e, Four (f, g, h, i)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), t2)

and addDigits2 (t1, d1, x, y, d2, t2) =
   case (d1, x, y, d2) of
      (One a, b, c, One d) => appTree2 (t1, node2' (a, b), node2' (c, d), t2)
    | (One a, b, c, Two (d, e)) => appTree2 (t1, node3' (a, b, c), node2' (d, e), t2)
    | (One a, b, c, Three (d, e, f)) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)
    | (One a, b, c, Four (d, e, f, g)) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)

    | (Two (a, b), c, d, One e) => appTree2 (t1, node3' (a, b, c), node2' (d, e), t2)
    | (Two (a, b), c, d, Two (e, f)) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)
    | (Two (a, b), c, d, Three (e, f, g)) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)
    | (Two (a, b), c, d, Four (e, f, g, h)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)

    | (Three (a, b, c), d, e, One f) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)
    | (Three (a, b, c), d, e, Two (f, g)) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)
    | (Three (a, b, c), d, e, Three (f, g, h)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)
    | (Three (a, b, c), d, e, Four (f, g, h, i)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), t2)

    | (Four (a, b, c, d), e, f, One g) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)
    | (Four (a, b, c, d), e, f, Two (g, h)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)
    | (Four (a, b, c, d), e, f, Three (g, h, i)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), t2)
    | (Four (a, b, c, d), e, f, Four (g, h, i, j)) => appTree4 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), node2' (i, j), t2)

and addDigits3 (t1, d1, x, y, z, d2, t2) =
   case (d1, x, y, z, d2) of
      (One a, b, c, d, One e) => appTree2 (t1, node3' (a, b, c), node2' (d, e), t2)
    | (One a, b, c, d, Two (e, f)) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)
    | (One a, b, c, d, Three (e, f, g)) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)
    | (One a, b, c, d, Four (e, f, g, h)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)

    | (Two (a, b), c, d, e, One f) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)
    | (Two (a, b), c, d, e, Two (f, g)) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)
    | (Two (a, b), c, d, e, Three (f, g, h)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)
    | (Two (a, b), c, d, e, Four (f, g, h, i)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), t2)

    | (Three (a, b, c), d, e, f, One g) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)
    | (Three (a, b, c), d, e, f, Two (g, h)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)
    | (Three (a, b, c), d, e, f, Three (g, h, i)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), t2)
    | (Three (a, b, c), d, e, f, Four (g, h, i, j)) =>  appTree4 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), node2' (i, j), t2)

    | (Four (a, b, c, d), e, f, g, One h) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)
    | (Four (a, b, c, d), e, f, g, Two (h, i)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), t2)
    | (Four (a, b, c, d), e, f, g, Three (h, i, j)) => appTree4 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), node2' (i, j), t2)
    | (Four (a, b, c, d), e, f, g, Four (h, i, j, k)) => appTree4 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), node2' (j, k), t2)

and addDigits4 (t1, d1, x, y, z, w, d2, t2) =
   case (d1, x, y, z, w, d2) of
      (One a, b, c, d, e, One f) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)
    | (One a, b, c, d, e, Two (f, g)) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)
    | (One a, b, c, d, e, Three (f, g, h)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)
    | (One a, b, c, d, e, Four (f, g, h, i)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), t2)

    | (Two (a, b), c, d, e, f, One g) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)
    | (Two (a, b), c, d, e, f, Two (g, h)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)
    | (Two (a, b), c, d, e, f, Three (g, h, i)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), t2)
    | (Two (a, b), c, d, e, f, Four (g, h, i, j)) => appTree4 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), node2' (i, j), t2)

    | (Three (a, b, c), d, e, f, g, One h) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)
    | (Three (a, b, c), d, e, f, g, Two (h, i)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), t2)
    | (Three (a, b, c), d, e, f, g, Three (h, i, j)) => appTree4 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), node2' (i, j), t2)
    | (Three (a, b, c), d, e, f, g, Four (h, i, j, k)) => appTree4 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), node2' (j, k), t2)

    | (Four (a, b, c, d), e, f, g, h, One i) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), t2)
    | (Four (a, b, c, d), e, f, g, h, Two (i, j)) => appTree4 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), node2' (i, j), t2)
    | (Four (a, b, c, d), e, f, g, h, Three (i, j, k)) => appTree4 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), node2' (j, k), t2)
    | (Four (a, b, c, d), e, f, g, h, Four (i, j, k, l)) => appTree4 (t1, node3' (a, b, c), node3' (d, e, f), node3' (g, h, i), node3' (j, k, l), t2)

and appTree1 (Empty, a, ys) = consTree (a, ys)
  | appTree1 (xs, a, Empty) = snocTree (xs, a)
  | appTree1 (Single x, a, ys) = consTree (x, consTree (a, ys))
  | appTree1 (xs, a, Single y) = snocTree (snocTree (xs, a), y)
  | appTree1 (Deep (_, pr, m, sf), a, Deep (_, pr', m', sf')) =
   deep (pr,
         Lazy.delay (fn () =>
            addDigits1 (Lazy.force m,
                        sf,
                        a,
                        pr',
                        Lazy.force m')),
         sf')

and appTree2 (Empty, a, b, ys) = consTree (a, consTree (b, ys))
  | appTree2 (xs, a, b, Empty) = snocTree (snocTree (xs, a), b)
  | appTree2 (Single x, a, b, ys) = consTree (x, consTree (a, consTree (b, ys)))
  | appTree2 (xs, a, b, Single y) = snocTree (snocTree (snocTree (xs, a), b), y)
  | appTree2 (Deep (_, pr, m, sf), a, b, Deep (_, pr', m', sf')) =
   deep (pr,
         Lazy.delay (fn () =>
            addDigits2 (Lazy.force m,
                        sf,
                        a, b,
                        pr',
                        Lazy.force m')),
         sf')

and appTree3 (Empty, a, b, c, ys) = consTree (a, consTree (b, consTree (c, ys)))
  | appTree3 (xs, a, b, c, Empty) = snocTree (snocTree (snocTree (xs, a), b), c)
  | appTree3 (Single x, a, b, c, ys) = consTree (x, consTree (a, consTree (b, consTree (c, ys))))
  | appTree3 (xs, a, b, c, Single y) = snocTree (snocTree (snocTree (snocTree (xs, a), b), c), y)
  | appTree3 (Deep (_, pr, m, sf), a, b, c, Deep (_, pr', m', sf')) =
   deep (pr,
         Lazy.delay (fn () =>
            addDigits3 (Lazy.force m, sf, a, b, c, pr', Lazy.force m')),
         sf')

and appTree4 (Empty, a, b, c, d, ys) = consTree (a, consTree (b, consTree (c, consTree (d, ys))))
  | appTree4 (xs, a, b, c, d, Empty) = snocTree (snocTree (snocTree (snocTree (xs, a), b), c), d)
  | appTree4 (Single x, a, b, c, d, ys) = consTree (x, consTree (a, consTree (b, consTree (c, consTree (d, ys)))))
  | appTree4 (xs, a, b, c, d, Single y) = snocTree (snocTree (snocTree (snocTree (snocTree (xs, a), b), c), d), y)
  | appTree4 (Deep (_, pr, m, sf), a, b, c, d, Deep (_, pr', m', sf')) =
   deep (pr,
         Lazy.delay (fn () =>
            addDigits4 (Lazy.force m, sf, a, b, c, d, pr', Lazy.force m')),
         sf')

fun addDigits0 (t1, d1, d2, t2) =
   case (d1, d2) of
      (One a, One b) => appTree1 (t1, node2' (a, b), t2)
    | (One a, Two (b, c)) => appTree1 (t1, node3' (a, b, c), t2)
    | (One a, Three (b, c, d)) => appTree2 (t1, node2' (a, b), node2' (c, d), t2)
    | (One a, Four (b, c, d, e)) => appTree2 (t1, node3' (a, b, c), node2' (d, e), t2)

    | (Two (a, b), One c) => appTree1 (t1, node3' (a, b, c), t2)
    | (Two (a, b), Two (c, d)) => appTree2 (t1, node2' (a, b), node2' (c, d), t2)
    | (Two (a, b), Three (c, d, e)) => appTree2 (t1, node3' (a, b, c), node2' (d, e), t2)
    | (Two (a, b), Four (c, d, e, f)) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)

    | (Three (a, b, c), One d) => appTree2 (t1, node2' (a, b), node2' (c, d), t2)
    | (Three (a, b, c), Two (d, e)) => appTree2 (t1, node3' (a, b, c), node2' (d, e), t2)
    | (Three (a, b, c), Three (d, e, f)) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)
    | (Three (a, b, c), Four (d, e, f, g)) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)

    | (Four (a, b, c, d), One e) => appTree2 (t1, node3' (a, b, c), node2' (d, e), t2)
    | (Four (a, b, c, d), Two (e, f)) => appTree2 (t1, node3' (a, b, c), node3' (d, e, f), t2)
    | (Four (a, b, c, d), Three (e, f, g)) => appTree3 (t1, node3' (a, b, c), node2' (d, e), node2' (f, g), t2)
    | (Four (a, b, c, d), Four (e, f, g, h)) => appTree3 (t1, node3' (a, b, c), node3' (d, e, f), node2' (g, h), t2)

fun append (Empty, ys) = ys
  | append (xs, Empty) = xs
  | append (Single x, ys) = consTree (x, ys)
  | append (xs, Single y) = snocTree (xs, y)
  | append (Deep (_, pr, m, sf), Deep (_, pr', m', sf')) =
   deep (pr,
         Lazy.delay (fn () =>
            addDigits0 (Lazy.force m, sf, pr', Lazy.force m')),
         sf')

(* Searching *)

(* Result of `search`
 * `Position (l, x, r)`: predicate is false on `l` and true on `r`, and `x` is the first true
 * `OnLeft`: predicate is true everywhere
 * `OnRight`: predicate is false everywhere
 * `Nowhere`: predicate is not monotonic
 *)
datatype 'a search_result =
   Position of 'a t * 'a Item.t * 'a t
 | OnLeft
 | OnRight
 | Nowhere

(* Note: tree is forced prior to search/split, so don't worry about laziness *)

fun deepL (NONE, m, sf) = rotL (m, sf)
  | deepL (SOME pr, m, sf) = deep (pr, Lazy.eager m, sf)

fun deepR (pr, m, NONE) = rotR (pr, m)
  | deepR (pr, m, SOME sf) = deep (pr, Lazy.eager m, sf)

fun searchDigit _ (vl, One a, vr) = (NONE, a, NONE)
  | searchDigit p (vl, Two (a, b), vr) =
   let
      val vla = vl <+> measureElem a
      val vbr = measureElem b <+> vr
   in
      if p (vla, vbr)
         then (NONE, a, SOME (One b))
      else (SOME (One a), b, NONE)
   end
  | searchDigit p (vl, Three (a, b, c), vr) =
   let
      val vla = vl <+> measureElem a
      val vlab = vla <+> measureElem b
      val vcr = measureElem c <+> vr
      val vbcr = measureElem b <+> vcr
   in
      if p (vla, vbcr)
         then (NONE, a, SOME (Two (b, c)))
      else if p (vlab, vcr)
         then (SOME (One a), b, SOME (One c))
      else (SOME (Two (a, b)), c, NONE)
   end
  | searchDigit p (vl, Four (a, b, c, d), vr) =
   let
      val vla = vl <+> measureElem a
      val vlab = vla <+> measureElem b
      val vlabc = vlab <+> measureElem c
      val vdr = measureElem d <+> vr
      val vcdr = measureElem c <+> vdr
      val vbcdr = measureElem b <+> vcdr
   in
      if p (vla, vbcdr)
         then (NONE, a, SOME (Three (b, c, d)))
      else if p (vlab, vcdr)
         then (SOME (One a), b, SOME (Two (c, d)))
      else if p (vlabc, vdr)
         then (SOME (Two (a, b)), c, SOME (One d))
      else (SOME (Three (a, b, c)), d, NONE)
   end

fun searchNode p (vl, Node2 (_, a, b), vr) =
   let
      val vla = vl <+> measureElem a
      val vbr = measureElem b <+> vr
   in
      if p (vla, vbr)
         then (NONE, a, SOME (One b))
      else (SOME (One a), b, NONE)
   end
  | searchNode p (vl, Node3 (_, a, b, c), vr) =
   let
      val vla = vl <+> measureElem a
      val vlab = vla <+> measureElem b
      val vcr = measureElem c <+> vr
      val vbcr = measureElem b <+> vcr
   in
      if p (vla, vbcr)
         then (NONE, a, SOME (Two (b, c)))
      else if p (vlab, vcr)
         then (SOME (One a), b, SOME (One c))
      else (SOME (Two (a, b)), c, NONE)
   end

fun searchTree _ (_, Empty, _) = raise Fail "searchTree: Empty"
  | searchTree _ (_, Single x, _) = (Empty, x, Empty)
  | searchTree p (vl, Deep (_, pr, m, sf), vr) =
   let
      val m = Lazy.force m
      val vm = measure m
      val vlp = vl <+> measureDigit pr
      val vlpm = vlp <+> vm
      val vsr = measureDigit sf <+> vr
      val vmsr = vm <+> vsr
   in
      if p (vlp, vmsr)
         then
            case searchDigit p (vl, pr, vmsr) of
               (SOME l, x, r) => (digitToTree l, x, deepL (r, m, sf))
             | (NONE, x, r) => (Empty, x, deepL (r, m, sf))
      else if p (vlpm, vsr)
         then 
            case searchTree p (vlp, m, vsr) of
               (_, Item _, _) => raise Fail "searchTree: Item"
             | (ml, Node xs, mr) =>
                  case searchNode p (vlp <+> measure ml, xs, measure mr <+> vsr) of
                     (l, x, r) => (deepR (pr, ml, l), x, deepL (r, mr, sf))
      else 
         case searchDigit p (vlpm, sf, vr) of
            (l, x, SOME r) => (deepR (pr, m, l), x, digitToTree r)
          | (l, x, NONE) => (deepR (pr, m, l), x, Empty)
   end

fun search (p: Measure.t * Measure.t -> bool) (t: 'a t): 'a search_result =
   let
      val vt = measure t
      val pl = p (Measure.zero, vt)
      val pr = p (vt, Measure.zero)
   in
      case (pl, pr) of
         (true, true) => OnLeft
       | (false, false) => OnRight
       | (true, false) => Nowhere
       | (false, true) =>
            case searchTree p (Measure.zero, t, Measure.zero) of
               (l, Item x, r) => Position (l, x, r)
             | (_, Node _, _) => raise Fail "search: Node"
   end

fun splitDigit _ (vl, One a) = (NONE, a, NONE)
  | splitDigit p (vl, Two (a, b)) =
   let
      val vla = vl <+> measureElem a
   in
      if p vla
         then (NONE, a, SOME (One b))
      else (SOME (One a), b, NONE)
   end
  | splitDigit p (vl, Three (a, b, c)) =
   let
      val vla = vl <+> measureElem a
      val vlab = vla <+> measureElem b
   in
      if p vla
         then (NONE, a, SOME (Two (b, c)))
      else if p vlab
         then (SOME (One a), b, SOME (One c))
      else (SOME (Two (a, b)), c, NONE)
   end
  | splitDigit p (vl, Four (a, b, c, d)) =
   let
      val vla = vl <+> measureElem a
      val vlab = vla <+> measureElem b
      val vlabc = vlab <+> measureElem c
   in
      if p vla
         then (NONE, a, SOME (Three (b, c, d)))
      else if p vlab
         then (SOME (One a), b, SOME (Two (c, d)))
      else if p vlabc
         then (SOME (Two (a, b)), c, SOME (One d))
      else (SOME (Three (a, b, c)), d, NONE)
   end

fun splitNode p (vl, Node2 (_, a, b)) =
   let
      val vla = vl <+> measureElem a
   in
      if p vla
         then (NONE, a, SOME (One b))
      else (SOME (One a), b, NONE)
   end
  | splitNode p (vl, Node3 (_, a, b, c)) =
   let
      val vla = vl <+> measureElem a
      val vlab = vla <+> measureElem b
   in
      if p vla
         then (NONE, a, SOME (Two (b, c)))
      else if p vlab
         then (SOME (One a), b, SOME (One c))
      else (SOME (Two (a, b)), c, NONE)
   end

fun splitTree _ (_, Empty) = raise Fail "searchTree: Empty"
  | splitTree _ (_, Single x) = (Empty, x, Empty)
  | splitTree p (vl, Deep (_, pr, m, sf)) =
   let
      val m = Lazy.force m
      val vlp = vl <+> measureDigit pr
      val vlpm = vlp <+> measure m
   in
      if p vlp
         then
            case splitDigit p (vl, pr) of
               (SOME l, x, r) => (digitToTree l, x, deepL (r, m, sf))
             | (NONE, x, r) => (Empty, x, deepL (r, m, sf))
      else if p vlpm
         then 
            case splitTree p (vlp, m) of
               (_, Item _, _) => raise Fail "searchTree: Item"
             | (ml, Node xs, mr) =>
                  case splitNode p (vlp <+> measure ml, xs) of
                     (l, x, r) => (deepR (pr, ml, l), x, deepL (r, mr, sf))
      else 
         case splitDigit p (vlpm, sf) of
            (l, x, SOME r) => (deepR (pr, m, l), x, digitToTree r)
          | (l, x, NONE) => (deepR (pr, m, l), x, Empty)
   end

fun split _ Empty = (Empty, Empty)
  | split p t =
   if p (measure t)
      then
         case splitTree p (Measure.zero, t) of
            (l, x, r) => (l, consTree (x, r))
   else (t, Empty)

(* Transformations *)

fun reverseNode (Node2 (v, a, b)) = Node2 (v, reverseElem b, reverseElem a)
  | reverseNode (Node3 (v, a, b, c)) = Node3 (v, reverseElem c, reverseElem b, reverseElem a)

and reverseElem (Item x) = Item x
  | reverseElem (Node n) = Node (reverseNode n)

fun reverseDigit (One a) = One (reverseElem a)
  | reverseDigit (Two (a, b)) = Two (reverseElem b, reverseElem a)
  | reverseDigit (Three (a, b, c)) = Three (reverseElem c, reverseElem b, reverseElem a)
  | reverseDigit (Four (a, b, c, d)) = Four (reverseElem d, reverseElem c, reverseElem b, reverseElem a)

fun reverse Empty = Empty
  | reverse (Single x) = Single x
  | reverse (Deep (_, pr, m, sf)) = deep (reverseDigit sf, Lazy.map reverse m, reverseDigit pr)

end

structure Seq = FingerTree(
   struct
      structure Measure =
         struct
            type t = int
            val op + = Int.+
            val zero = 0
         end

      structure Item =
         struct
            type 'a t = 'a
            fun measure _ = 1
         end
   end)

(* vim: set ft=sml ts=3 sw=3 tw=0: *)
