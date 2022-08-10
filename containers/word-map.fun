
(* Big-endian patricia tries *)
functor WordMapFn(W:
   sig
      include WORD

      val same: word * word -> bool

      (* Returns a word with one bit set
       * The set bit is the largest set bit in the input word
       * On an input of 0, output is undefined *)
      val highestBitMask: word -> word
   end) =
   (* end): WORD_MAP where type key = W.word = *)
struct

type key = W.word

(* Invariants:
 *
 * - `Nil` only appears at toplevel; never as a subtree of `Bin`
 * - Given `Bin (prefix, mask, l, r)`:
 *    + `prefix` does not have any set bits equal to or below the `mask` bit
 *    + For any key `k` in `l` or `r`,
 *      the bits above the `mask` bit agree with `prefix`.
 *      Ex. if k=0b1001 and m=0b0010, then prefix must be 0b1000
 *    + Keys in `l` do not have the `mask` bit set
 *    + Keys in `r` have the `mask` bit set
 *)
datatype 'a map =
   Nil
 | Tip of key * 'a
 | Bin of W.word * W.word * 'a map * 'a map

(* Given a bitmask `m` with one bit set (say `m = 0w1 << n`),
 * zeroes out the lower `n + 1` bits of `k`
 *
 * Ex. `mask (0wxff, 0w1 << 0w4) = 0wxe0` (upper 3 bits of 0wxff)
 *)
fun mask (k, m) = W.andb (k, W.xorb (m, W.+ (W.notb m, W.fromInt 1)))

(* Create a `Bin` node for the subtrees `t1` and `t2`
 * `p1` and `p2` are the common prefixes of the respective trees
 * Note: The prefixes `p1` and `p2` *must* be different
 *)
fun link (p1, t1, p2, t2) =
   let
      val m = W.highestBitMask (W.xorb (p1, p2))
      val p = mask (p1, m)
   in
      if W.same (W.andb (p1, m), W.fromInt 0)
         then Bin (p, m, t1, t2)
      else Bin (p, m, t2, t1)
   end

(* Returns true if the key `k` does not match the prefix-mask pair `p`, `m`
 * Checks the upper bits of `k` against `p`,
 * where the number of bits is decided by `m`
 *)
fun nomatch (k, p, m) = not (W.same (mask (k, m), p))

(* Returns true if the mask bit given by `m` is zero in the key `k` *)
fun zero (k, m) = W.same (W.andb (k, m), W.fromInt 0)

(* Determine which mask denotes a smaller key prefix
 * A mask is shorter if the value is larger, since we use big-endian
 *)
val shorter = W.>

(* Smart-constructors: check for empty subtrees *)
fun bin (_, _, Nil, r) = r
  | bin (_, _, l, Nil) = l
  | bin (p, m, l, r) = Bin (p, m, l, r)

fun binCheckL (_, _, Nil, r) = r
  | binCheckL (p, m, l, r) = Bin (p, m, l, r)

fun binCheckR (_, _, l, Nil) = l
  | binCheckR (p, m, l, r) = Bin (p, m, l, r)

(** Exports **)

val empty = Nil

val singleton = Tip

fun insertLookupWithi f (t, k, x) =
   let
      fun mapSnd f (a, b) = (a, f b)

      fun go Nil = (NONE, Tip (k, x))
        | go (t as Tip (k', x')) =
         if W.same (k, k')
            then (SOME x', Tip (k, f (k, x', x)))
         else (NONE, link (k', t, k, Tip (k, x)))
        | go (t as Bin (p, m, l, r)) =
         if nomatch (k, p, m)
            then (NONE, link (p, t, k, Tip (k, x)))
         else if zero (k, m)
            then mapSnd (fn l' => Bin (p, m, l', r)) (go l)
         else mapSnd (fn r' => Bin (p, m, l, r')) (go r)
   in
      go t
   end

fun insertWithi f (t, k, x) =
   let
      fun go Nil = Tip (k, x)
        | go (t as Tip (k', x')) =
         if W.same (k, k')
            then Tip (k, f (k, x', x))
         else link (k', t, k, Tip (k, x))
        | go (t as Bin (p, m, l, r)) =
         if nomatch (k, p, m)
            then link (p, t, k, Tip (k, x))
         else if zero (k, m)
            then Bin (p, m, go l, r)
         else Bin (p, m, l, go r)
   in
      go t
   end

fun insertWith f = insertWithi (fn (_, x', x) => f (x', x))

fun insert (t, k, x) = insertWithi (fn (_, _, x) => x) (t, k, x)

fun insert' ((k, x), t) = insert (t, k, x)

fun fromList xs = List.foldl insert' empty xs

fun fromListWithi f xs =
   List.foldl (fn ((k, x), t) => insertWithi f (t, k, x)) empty xs

fun fromListWith f xs =
   List.foldl (fn ((k, x), t) => insertWith f (t, k, x)) empty xs

(** Deletion/Update **)

fun remove (t, k) =
   let
      fun go Nil = NONE
        | go (Tip (k', x')) =
         if W.same (k, k')
            then SOME (Nil, x')
         else NONE
        | go (Bin (p, m, l, r)) =
         if nomatch (k, p, m)
            then NONE
         else if zero (k, m)
            then
               case go l of
                  NONE => NONE
                | SOME (Nil, x) => SOME (r, x)
                | SOME (l', x) => SOME (Bin (p, m, l', r), x)
         else
            case go r of
               NONE => NONE
             | SOME (Nil, x) => SOME (l, x)
             | SOME (r', x) => SOME (Bin (p, m, l, r'), x)
   in
      go t
   end

fun delete (t, k) =
   case remove (t, k) of
      NONE => t
    | SOME (t', _) => t'

fun adjust f (t, k) =
   let
      fun go Nil = Nil
        | go (t as Tip (k', x')) =
         if W.same (k, k')
            then Tip (k, f x')
         else t
        | go (t as Bin (p, m, l, r)) =
         if nomatch (k, p, m)
            then t
         else if zero (k, m)
            then Bin (p, m, go l, r)
         else Bin (p, m, l, go r)
   in
      go t
   end

fun update f (t, k) =
   let
      fun go Nil = Nil
        | go (t as Tip (k', x')) =
         if W.same (k, k')
            then
               case f x' of
                  NONE => Nil
                | SOME y => Tip (k, y)
         else t
        | go (t as Bin (p, m, l, r)) =
         if nomatch (k, p, m)
            then t
         else if zero (k, m)
            then binCheckL (p, m, go l, r)
         else binCheckR (p, m, l, go r)
   in
      go t
   end

fun updateLookupWithi f (t, k) =
   let
      fun go Nil = (NONE, Nil)
        | go (t as Tip (k', x')) =
         if W.same (k, k')
            then
               case f (k, x') of
                  NONE => (SOME x', Nil)
                | SOME y => (SOME x', Tip (k, y))
         else (NONE, t)
        | go (t as Bin (p, m, l, r)) =
         if nomatch (k, p, m)
            then (NONE, t)
         else if zero (k, m)
            then 
               case go l of
                  (ox, Nil) => (ox, r)
                | (ox, l') => (ox, Bin (p, m, l', r))
         else 
            case go r of
               (ox, Nil) => (ox, l)
             | (ox, r') => (ox, Bin (p, m, l, r'))
   in
      go t
   end

fun alter f (t, k) =
   let
      fun go Nil =
         (case f NONE of
            SOME x => Tip (k, x)
          | NONE => Nil)
        | go (t as Tip (k', x')) =
         if W.same (k, k')
            then
               case f (SOME x') of
                  SOME y => Tip (k, y)
                | NONE => Nil
         else
            (case f NONE of
               SOME y => link (k, Tip (k, y), k', t)
             | NONE => t)
        | go (t as Bin (p, m, l, r)) =
         if nomatch (k, p, m)
            then
               case f NONE of
                  SOME x => link (k, Tip (k, x), p, t)
                | NONE => t
         else if zero (k, m)
            then binCheckL (p, m, go l, r)
         else binCheckR (p, m, l, go r)
   in
      go t
   end

(** Lookup **)

fun find (t, k) =
   let
      fun go Nil = NONE
        | go (Tip (k', x')) =
         if W.same (k, k')
            then SOME x'
         else NONE
        | go (Bin (p, m, l, r)) =
         if nomatch (k, p, m)
            then NONE
         else if zero (k, m)
            then go l
         else go r
   in
      go t
   end

fun inDomain (t, k) = Option.isSome (find (t, k))

(** Size **)

fun isEmpty Nil = true
  | isEmpty _ = false

fun size t =
   let
      fun go (Nil, acc) = acc
        | go (Tip _, acc) = acc + 1
        | go (Bin (_, _, l, r), acc) = go (l, go (r, acc))
   in
      go (t, 0)
   end

(** Union **)

fun unionWithi f =
   let
      fun f' (k, x2, x1) = f (k, x1, x2)

      fun go (Nil, t2) = t2
        | go (t1, Nil) = t1
        | go (Tip (k1, x1), t2) = insertWithi f' (t2, k1, x1)
        | go (t1, Tip (k2, x2)) = insertWithi f (t1, k2, x2)
        | go (t1 as Bin (p1, m1, l1, r1), t2 as Bin (p2, m2, l2, r2)) =
         if shorter (m1, m2)
            then
               if nomatch (p2, p1, m1)
                  then link (p1, t1, p2, t2)
               else if zero (p2, m1)
                  then Bin (p1, m1, go (l1, t2), r1)
               else Bin (p1, m1, l1, go (r1, t2))
         else if shorter (m2, m1)
            then
               if nomatch (p1, p2, m2)
                  then link (p1, t1, p2, t2)
               else if zero (p1, m2)
                  then Bin (p2, m2, go (t1, l2), r2)
               else Bin (p2, m2, l2, go (t1, r2))
         else if W.same (p1, p2)
            then Bin (p1, m1, go (l1, l2), go (r1, r2))
         else link (p1, t1, p2, t2)
   in
      go
   end

fun unionWith f = unionWithi (fn (_, x, x') => f (x, x'))

(** Difference **)

fun differenceWithi f =
   let
      fun go (Nil, _) = Nil
        | go (t1, Nil) = t1
        | go (Tip (k1, x1), t2) =
         (case find (t2, k1) of
            NONE => Tip (k1, x1)
          | SOME x2 =>
               case f (k1, x1, x2) of
                  NONE => Nil
                | SOME x => Tip (k1, x))
        | go (t1, Tip (k2, x2)) = update (fn x1 => f (k2, x1, x2)) (t1, k2)
        | go (t1 as Bin (p1, m1, l1, r1), t2 as Bin (p2, m2, l2, r2)) =
         if shorter (m1, m2)
            then
               if nomatch (p2, p1, m1)
                  then t1
               else if zero (p2, m1)
                  then binCheckL (p1, m1, go (l1, t2), r1)
               else binCheckR (p1, m1, l1, go (r1, t2))
         else if shorter (m2, m1)
            then
               if nomatch (p1, p2, m2)
                  then t1
               else if zero (p1, m2)
                  then go (t1, l2)
               else go (t1, r2)
         else if W.same (p1, p2)
            then bin (p1, m1, go (l1, l2), go (r1, r2))
         else t1
   in
      go
   end

fun differenceWith f = differenceWithi (fn (_, x, x') => f (x, x'))

fun difference (t1, t2) = differenceWithi (fn _ => NONE) (t1, t2)

(** Intersection **)

fun intersectionWithi f =
   let
      fun go (Nil, _) = Nil
        | go (_, Nil) = Nil
        | go (Tip (k1, x1), t2) =
         (case find (t2, k1) of
            NONE => Nil
          | SOME x2 =>
               case f (k1, x1, x2) of
                  NONE => Nil
                | SOME y => Tip (k1, y))
        | go (t1, Tip (k2, x2)) =
         (case find (t1, k2) of
            NONE => Nil
          | SOME x1 =>
               case f (k2, x1, x2) of
                  NONE => Nil
                | SOME y => Tip (k2, y))
        | go (t1 as Bin (p1, m1, l1, r1), t2 as Bin (p2, m2, l2, r2)) =
         if shorter (m1, m2)
            then
               if nomatch (p2, p1, m1)
                  then Nil
               else if zero (p2, m1)
                  then go (l1, t2)
               else go (r1, t2)
         else if shorter (m2, m1)
            then
               if nomatch (p1, p2, m2)
                  then Nil
               else if zero (p1, m2)
                  then go (t1, l2)
               else go (t1, r2)
         else if W.same (p1, p2)
            then bin (p1, m1, go (l1, l2), go (r1, r2))
         else Nil
   in
      go
   end

fun intersectionWith f = intersectionWithi (fn (_, x, x') => f (x, x'))

fun intersection (t1, t2) = intersectionWithi (fn (_, x, _) => SOME x) (t1, t2)

fun disjoint (Nil, _) = true
  | disjoint (_, Nil) = true
  | disjoint (Tip (k1, _), t2) = not (inDomain (t2, k1))
  | disjoint (t1, Tip (k2, _)) = not (inDomain (t1, k2))
  | disjoint (t1 as Bin (p1, m1, l1, r1), t2 as Bin (p2, m2, l2, r2)) =
   if shorter (m1, m2)
      then
         if nomatch (p2, p1, m1)
            then true
         else if zero (p2, m1)
            then disjoint (l1, t2)
         else disjoint (r1, t2)
   else if shorter (m2, m1)
      then
         if nomatch (p1, p2, m2)
            then true
         else if zero (p1, m2)
            then disjoint (t1, l2)
         else disjoint (t1, r2)
   else if W.same (p1, p2)
      then disjoint (l1, l2) andalso disjoint (r1, r2)
   else true

(** Traversal **)

fun mapi _ Nil = Nil
  | mapi f (Tip (k, x)) = Tip (k, f (k, x))
  | mapi f (Bin (p, m, l, r)) = Bin (p, m, mapi f l, mapi f r)

fun map f = mapi (fn (_, x) => f x)

fun mapAccumLi f z t =
   let
      fun go (Nil, acc) = (acc, Nil)
        | go (Tip (k, x), acc) =
         let
            val (acc, x) = f (k, x, acc)
         in
            (acc, Tip (k, x))
         end
        | go (Bin (p, m, l, r), acc) =
         let
            val (acc, l) = go (l, acc)
            val (acc, r) = go (r, acc)
         in
            (acc, Bin (p, m, l, r))
         end
   in
      go (t, z)
   end

fun mapAccumL f = mapAccumLi (fn (_, x, acc) => f (x, acc))

fun mapAccumRi f z t =
   let
      fun go (Nil, acc) = (acc, Nil)
        | go (Tip (k, x), acc) =
         let
            val (acc, x) = f (k, x, acc)
         in
            (acc, Tip (k, x))
         end
        | go (Bin (p, m, l, r), acc) =
         let
            val (acc, r) = go (r, acc)
            val (acc, l) = go (l, acc)
         in
            (acc, Bin (p, m, l, r))
         end
   in
      go (t, z)
   end

fun mapAccumR f = mapAccumRi (fn (_, x, acc) => f (x, acc))

fun foldli f z t =
   let
      fun go (Nil, acc) = acc
        | go (Tip (k, x), acc) = f (k, x, acc)
        | go (Bin (_, _, l, r), acc) = go (r, go (l, acc))
   in
      go (t, z)
   end

fun foldl f = foldli (fn (_, x, acc) => f (x, acc))

fun foldri f z t =
   let
      fun go (Nil, acc) = acc
        | go (Tip (k, x), acc) = f (k, x, acc)
        | go (Bin (_, _, l, r), acc) = go (l, go (r, acc))
   in
      go (t, z)
   end

fun foldr f = foldri (fn (_, x, acc) => f (x, acc))

fun appi _ Nil = ()
  | appi f (Tip (k, x)) = f (k, x)
  | appi f (Bin (_, _, l, r)) = (appi f l; appi f r)

fun app f = appi (fn (_, x) => f x)

fun existsi _ Nil = false
  | existsi f (Tip (k, x)) = f (k, x)
  | existsi f (Bin (_, _, l, r)) = existsi f l orelse existsi f r

fun exists f = existsi (fn (_, x) => f x)

fun alli _ Nil = true
  | alli f (Tip (k, x)) = f (k, x)
  | alli f (Bin (_, _, l, r)) = alli f l andalso alli f r

fun all f = alli (fn (_, x) => f x)

(** Conversion **)

fun keys t = foldri (fn (k, _, acc) => k :: acc) [] t

fun elems t = foldr op :: [] t

fun toList t = foldri (fn (k, x, acc) => (k, x) :: acc) [] t

(** Filter **)

fun filteri p =
   let
      fun go Nil = Nil
        | go (t as Tip (k, x)) =
         if p (k, x)
            then t
         else Nil
        | go (Bin (p, m, l, r)) = bin (p, m, go l, go r)
   in
      go
   end

fun filter p = filteri (fn (_, x) => p x)

fun mapPartiali f =
   let
      fun go Nil = Nil
        | go (Tip (k, x)) =
         (case f (k, x) of
            NONE => Nil
          | SOME y => Tip (k, y))
        | go (Bin (p, m, l, r)) = bin (p, m, go l, go r)
   in
      go
   end

fun mapPartial f = mapPartiali (fn (_, x) => f x)

fun mapEitheri f =
   let
      fun go Nil = (Nil, Nil)
        | go (Tip (k, x)) =
         (case f (k, x) of
            Either.INL y => (Tip (k, y), Nil)
          | Either.INR z => (Nil, Tip (k, z)))
        | go (Bin (p, m, l, r)) =
         let
            val (l1, l2) = go l
            val (r1, r2) = go r
         in
            (bin (p, m, l1, r1), bin (p, m, l2, r2))
         end
   in
      go
   end

fun mapEither f = mapEitheri (fn (_, x) => f x)

fun partitioni p =
   let
      fun go Nil = (Nil, Nil)
        | go (t as Tip (k, x)) =
         if p (k, x)
            then (t, Nil)
         else (Nil, t)
        | go (Bin (p, m, l, r)) =
         let
            val (l1, l2) = go l
            val (r1, r2) = go r
         in
            (bin (p, m, l1, r1), bin (p, m, l2, r2))
         end
   in
      go
   end

fun partition p = partitioni (fn (_, x) => p x)

(** Submap **)

fun isSubmapBy _ (Nil, _) = true
  | isSubmapBy _ (_, Nil) = false
  | isSubmapBy pred (Tip (k, x), t) =
   (case find (t, k) of
      NONE => false
    | SOME y => pred (x, y))
  | isSubmapBy _ (Bin _, Tip _) = false
  | isSubmapBy pred (t1 as Bin (p1, m1, l1, r1), t2 as Bin (p2, m2, l2, r2)) =
   if shorter (m1, m2)
      then false
   else if shorter (m2, m1)
      then
         if nomatch (p1, p2, m2)
            then false
         else if zero (p1, m2)
            then isSubmapBy pred (t1, l2)
         else isSubmapBy pred (t1, r2)
   else
      W.same (p1, p2)
      andalso isSubmapBy pred (l1, l2)
      andalso isSubmapBy pred (r1, r2)

fun isProperSubmapBy pred (t1, t2) =
   let
      (* LESS -> proper submap
       * EQUAL -> equal
       * GREATER -> not a submap (disjoint) *)
      fun go (Nil, Nil) = EQUAL
        | go (Nil, _) = LESS
        | go (Tip (k, x), Tip (k', x')) =
         if W.same (k, k') andalso pred (x, x')
            then EQUAL
         else GREATER
        | go (Tip (k, x), t) =
         (case find (t, k) of
            NONE => GREATER
          | SOME y => if pred (x, y) then LESS else GREATER)
        | go (t1 as Bin (p1, m1, l1, r1), t2 as Bin (p2, m2, l2, r2)) =
         if shorter (m1, m2)
            then GREATER
         else if shorter (m2, m1)
            then
               if nomatch (p1, p2, m2)
                  then GREATER
               else if zero (p1, m2)
                  then go (t1, l2)
               else go (t1, r2)
         else if W.same (p1, m2)
            then
               case go (l1, l2) of
                  GREATER => GREATER
                | lcmp =>
                     case (lcmp, go (r1, r2)) of
                        (_, GREATER) => GREATER
                      | (EQUAL, EQUAL) => EQUAL
                      | _ => LESS
         else GREATER
        | go (Bin _, _) = GREATER
   in
      go (t1, t2) = LESS
   end

fun liftEquals _ (Nil, Nil) = true
  | liftEquals eq (Tip (k, x), Tip (k', x')) =
   W.same (k, k')
   andalso eq (x, x')
  | liftEquals eq (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =
   W.same (m1, m2)
   andalso W.same (p1, p2)
   andalso liftEquals eq (l1, l2)
   andalso liftEquals eq (r1, r2)
  | liftEquals _ (_, _) = false

(* TODO: Use pull streams instead of lists *)
fun collate cmp (t1, t2) =
   let
      fun go ([], []) = EQUAL
        | go ([], _ :: _) = LESS
        | go (_ :: _, []) = GREATER
        | go ((k, x) :: xs, (k', y) :: ys) =
         case W.compare (k, k') of
            EQUAL =>
               (case cmp (x, y) of
                  EQUAL => go (xs, ys)
                | order => order)
          | order => order
   in
      go (toList t1, toList t2)
   end

(** WordMap specific **)

fun viewMin Nil = NONE
  | viewMin t =
   let
      fun go Nil = raise Fail "WordMapFn.viewMin: invalid Nil"
        | go (Tip (k, x)) = (k, x, Nil)
        | go (Bin (p, m, l, r)) =
         case go l of
            (k, x, l') => (k, x, binCheckL (p, m, l', r))
   in
      SOME (go t)
   end

fun viewMax Nil = NONE
  | viewMax t =
   let
      fun go Nil = raise Fail "WordMapFn.viewMax: invalid Nil"
        | go (Tip (k, x)) = (k, x, Nil)
        | go (Bin (p, m, l, r)) =
         case go r of
              (k, x, r') => (k, x, binCheckR (p, m, l, r'))
   in
      SOME (go t)
   end

fun findMin t = Option.map (fn (k, x, _) => (k, x)) (viewMin t)
fun findMax t = Option.map (fn (k, x, _) => (k, x)) (viewMax t)
fun deleteMin t = Option.map #3 (viewMin t)
fun deleteMax t = Option.map #3 (viewMax t)

structure Fold =
   struct
      structure Base =
         struct
            datatype ('a, 'b) t =
               NilF
             | TipF of key * 'b
             | BinF of 'a * 'a

            fun map _ NilF = NilF
              | map _ (TipF (k, x)) = TipF (k, x)
              | map f (BinF (l, r)) = BinF (f l, f r)

            fun project Nil = NilF
              | project (Tip (k, x)) = TipF (k, x)
              | project (Bin (_, _, l, r)) = BinF (l, r)
         end

      structure Cofree =
         struct
            infixr 5 :<

            datatype ('a, 'b) t = :< of 'a * (('a, 'b) t, 'b) Base.t

            fun map f (a :< b) = f a :< Base.map (map f) b
            fun extract (a :< _) = a
            fun unwrap (_ :< b) = b
            fun duplicate w = w :< Base.map duplicate (unwrap w)
            fun extend f w = f w :< Base.map (extend f) (unwrap w)
         end

      fun cata f =
         let
            fun go x = f (Base.map go (Base.project x))
         in
            go
         end

      fun para f =
         let
            fun go x = f (Base.map go' (Base.project x))
            and go' x = (x, go x)
         in
            go
         end

      fun histo f =
         let
            fun dist w = Cofree.:< (Base.map Cofree.extract w, Base.map (dist o Cofree.unwrap) w)

            fun go x = dist (Base.map (Cofree.duplicate o Cofree.map f o go) (Base.project x))
         in
            f o Cofree.extract o go
         end

      fun zygo f g =
         let
            fun extract (_, b) = b
            fun duplicate (a, b) = (a, (a, b))
            fun map f (a, b) = (a, f b)

            fun dist w = (f (Base.map #1 w), Base.map extract w)

            fun go x = dist (Base.map (duplicate o map g o go) (Base.project x))
         in
            g o extract o go
         end
   end

(** Zipper **)
structure Zipper =
   struct
      datatype 'a crumb =
         BinL of W.word * W.word * 'a map
       | BinR of W.word * W.word * 'a map

      datatype 'a t = T of 'a map * 'a crumb list

      fun focus (T (t, _)) = t

      fun deleteFocus (T (_, ctx)) = T (Nil, ctx)

      fun mapFocus f (T (t, ctx)) = T (map f t, ctx)

      fun mapPartialFocus f (T (t, ctx)) = T (mapPartial f t, ctx)

      fun filterFocus f (T (t, ctx)) = T (filter f t, ctx)

      fun differenceFocus (T (t, ctx), t') = T (difference (t, t'), ctx)

      fun up (T (t, BinL (p, m, l) :: ctx)) = SOME (T (binCheckR (p, m, l, t), ctx))
        | up (T (t, BinR (p, m, r) :: ctx)) = SOME (T (binCheckL (p, m, t, r), ctx))
        | up _ = NONE

      fun downLeft (T (Bin (p, m, l, r), ctx)) = SOME (T (l, BinR (p, m, r) :: ctx))
        | downLeft _ = NONE

      fun downRight (T (Bin (p, m, l, r), ctx)) = SOME (T (r, BinL (p, m, l) :: ctx))
        | downRight _ = NONE

      fun tug f x = Option.getOpt (f x, x)

      fun tugs f n x =
         if n < 0
            then raise Fail "WordMapFn.Zipper.tugs: Negative tug count"
         else
            let
               fun go (0, x) = x
                 | go (n, x) =
                  case f x of
                     NONE => x
                   | SOME y => go (n - 1, y)
            in
               go (n, x)
            end

      fun furthest f =
         let
            fun go x =
               case f x of
                  NONE => x
                | SOME y => go y
         in
            go
         end

      fun jerks f n x =
         if n < 0
            then raise Fail "WordMapFn.Zipper.jerks: Negative jerk count"
         else
            let
               fun go (0, x) = SOME x
                 | go (n, x) =
                  case f x of
                     NONE => NONE
                   | SOME y => go (n - 1, y)
            in
               go (n, x)
            end

      fun fromMap t = T (t, [])

      fun toMap (T (t, [])) = t
        | toMap z = toMap (furthest up z)
   end
end

(* vim: set tw=0 ts=3 sw=3: *)
