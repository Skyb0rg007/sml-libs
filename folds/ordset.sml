

structure OrdSet:
sig

type 'a t

val empty: ('a * 'a -> order) -> 'a t
val insert: 'a t * 'a -> 'a t
val member: 'a t * 'a -> bool

end =
struct

structure Tree:
   sig
      datatype 'a t = Tip | Bin of int * 'a t * 'a * 'a t

      val balance: 'a t * 'a * 'a t -> 'a t
   end =
   struct
      datatype 'a t = Tip | Bin of int * 'a t * 'a * 'a t

      fun size Tip = 0
        | size (Bin (sz, _, _, _)) = sz

      fun bin (l, x, r) = Bin (size l + size r + 1, l, x, r)

      val ratio = 2
      val delta = 4

      fun singleL (l, x, Bin (_, rl, y, rr)) = bin (bin (l, x, rl), y, rr)
        | singleL _ = raise Fail "OrdSet.Tree.singleL"

      fun singleR (Bin (_, ll, y, lr), x, r) = bin (ll, y, bin (lr, x, r))
        | singleR _ = raise Fail "OrdSet.Tree.singleR"

      fun doubleL (l, x, Bin (_, Bin (_, rll, y, rlr), z, rr)) = bin (bin (l, x, rll), y, bin (rlr, z, rr))
        | doubleL _ = raise Fail "OrdSet.Tree.doubleL"

      fun doubleR (Bin (_, ll, y, Bin (_, lrl, z, lrr)), x, r) = bin (bin (ll, y, lrl), z, bin (lrr, x, r))
        | doubleR _ = raise Fail "OrdSet.Tree.doubleL"

      fun rotateL (l, x, r as Bin (_, rl, _, rr)) =
         if size rl < ratio * size rr
            then singleL (l, x, r)
         else doubleL (l, x, r)
        | rotateL _ = raise Fail "OrdSet.Tree.rotateL"

      fun rotateR (l as Bin (_, ll, _, lr), x, r) =
         if size ll < ratio * size lr
            then singleR (l, x, r)
         else doubleR (l, x, r)
        | rotateR _ = raise Fail "OrdSet.Tree.rotateR"

      fun balance (l, x, r) =
         let
            val sizeL = size l
            val sizeR = size r
            val sizeX = sizeL + sizeR + 1
         in
            if sizeL + sizeR <= 1
               then Bin (sizeX, l, x, r)
            else if sizeR >= delta * sizeL
               then rotateL (l, x, r)
            else if sizeL >= delta * sizeR
               then rotateR (l, x, r)
            else Bin (sizeX, l, x, r)
         end
   end

datatype 'a t = Set of ('a * 'a -> order) * 'a Tree.t

fun empty cmp = Set (cmp, Tree.Tip)

fun insert (Set (cmp, t), x) =
   let
      fun go Tree.Tip = Tree.Bin (1, Tree.Tip, x, Tree.Tip)
        | go (Tree.Bin (sz, l, y, r)) =
         case cmp (x, y) of
            LESS => Tree.balance (go l, y, r)
          | GREATER => Tree.balance (l, y, go r)
          | EQUAL => Tree.Bin (sz, l, x, r)
   in
      Set (cmp, go t)
   end

fun member (Set (cmp, t), x) =
   let
      fun go Tree.Tip = false
        | go (Tree.Bin (_, l, y, r)) =
         case cmp (x, y) of
            LESS => go l
          | GREATER => go r
          | EQUAL => true
   in
      go t
   end

end
(* vim: set ft=sml tw=0 sw=3 ts=3: *)
