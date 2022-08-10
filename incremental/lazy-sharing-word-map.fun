
functor LazySharingWordMapFn(S: LAZY_SHARING_WORD_MAP_STRUCTS): LAZY_SHARING_WORD_MAP =
struct
   infix << >> andb orb xorb

   structure DS = DisjointSet
   structure W = Word

   val op << = W.<<
   val op >> = W.>>
   val op andb = W.andb
   val op orb = W.orb
   val op xorb = W.xorb

   type key = word
   type value = S.t

   datatype node =
      Nil
    | Bin of word * word * t * t
    | Tip of key * value

   withtype t = node DS.t

   fun highestBitMask w =
     let
       val w = w orb (w >> 0w1)
       val w = w orb (w >> 0w2)
       val w = w orb (w >> 0w4)
       val w = w orb (w >> 0w8)
       val w = w orb (w >> 0w16)
       val w = w orb (w >> 0w32)
     in
       w xorb (w >> 0w1)
     end

   fun mask (k, m) = k andb (m xorb (W.notb m + 0w1))

   fun matches (k, p, m) = mask (k, m) = p

   fun link (p1, t1, p2, t2) =
      let
         val m = highestBitMask (p1 xorb p2)
         val p = mask (p1, m)
      in
         if (m andb p1) = 0w0
            then DS.make (Bin (p, m, t1, t2))
         else DS.make (Bin (p, m, t2, t1))
      end


   val empty = DS.make Nil

   val singleton = DS.make o Tip

   fun isEmpty t =
      case DS.! t of
         Nil => true
       | _ => false

   fun find (t, k) =
      case DS.! t of
         Nil => NONE
       | Tip (k', x) => if k = k' then SOME x else NONE
       | Bin (p, m, l, r) =>
            if (k andb (m xorb (W.notb m + 0w1))) <> p
               (* `k` doesn't match prefix *)
               then NONE
            else if (k andb m) = 0w0
               (* `k` has bit `m` unset, search left *)
               then find (l, k)
            else
               (* `k` has bit `m` set, search right *)
               find (r, k)

   fun same (m1, m2) =
      if DS.same (m1, m2)
         then true
      else if sameNode (DS.! m1, DS.! m2)
         then (DS.union (m1, m2); true)
      else false

   and sameNode (Nil, Nil) = true
     | sameNode (Tip (k, v), Tip (k', v')) = k = k' andalso S.same (v, v')
     | sameNode (Bin (p, m, l, r), Bin (p', m', l', r')) =
      p = p' andalso m = m' andalso same (l, l') andalso same (r, r')
     | sameNode _ = false

   fun insertWithi f (t, k, v) =
      let
         fun go t =
            case DS.! t of
               Nil => DS.make (Tip (k, v))
             | Tip (k', v') =>
                  if k = k'
                     then DS.make (Tip (k, f (k, v', v)))
                  else link (k, DS.make (Tip (k, v)), k', t)
             | Bin (p, m, l, r) =>
                  if not (matches (k, p, m))
                     then link (k, DS.make (Tip (k, v)), p, t)
                  else if (k andb m) = 0w0
                     then DS.make (Bin (p, m, go l, r))
                  else DS.make (Bin (p, m, l, go r))
      in
         go t
      end

   fun insertWith f = insertWithi (fn (_, v', v) => f (v', v))

   local
      fun f (_, _, v) = v
   in
      fun insert (t, k, v) = insertWithi f (t, k, v)
      fun insert' ((k, v), t) = insertWithi f (t, k, v)
   end

   fun fromList xs = List.foldl insert' empty xs

   (* The Lazy Structure sharing improves operations (A op B)
    * with property 1 and either 2a or 2b:
    *
    * 1. (A op ∅), (∅ op A), (A op A) are all constant-time
    * 2a. ∀C,
    *    - (A op B) = ((A ∩ C) op (B ∩ C)) ∪ ((A - C) op (B - C))
    *    - ((A ∩ C) op (B ∩ C)) ⊆ C
    * 2b. ∀C,
    *    - (A op B) can be computed in constant-time from values
    *      ((A ∩ C) op (B ∩ C)) and ((A - C) op (B - C))
    *)

   (* A ∪ A = A
    * A ∪ ∅ = A
    * ∅ ∪ A = A
    * ∀C, (A ∪ B) = ((A ∩ C) ∪ (B ∩ C)) ∪ ((A - C) ∪ (B - C))
    * Since C is chosen to be all keys with given bits set,
    * we can perform the union in constant-time with a `Bin` constructor.
    * *)
   fun unionWithi f (m1, m2) =
      let
         fun f' (k, x', x) = f (k, x, x')
         val tip = DS.make o Tip
         (* Don't need to check subtrees, since `∪` doesn't remove elements *)
         val bin = DS.make o Bin

         fun go (t1, t2) =
            if same (t1, t2)
               (* A ∪ A = A *)
               then t1
            else
               case (DS.! t1, DS.! t2) of
                  (* A ∪ ∅ = A *)
                  (_, Nil) => t1
                  (* ∅ ∪ A = A *)
                | (Nil, _) => t2
                | (Tip (k, x), Tip (k', x')) =>
                     if k = k'
                        then tip (k, f (k, x, x'))
                     else link (k, t1, k', t2)
                | (Tip (k, x), Bin (p, m, l, r)) =>
                     if not (matches (k, p, m))
                        then link (k, t1, p, t2)
                     else if (k andb m) = 0w0
                        then bin (p, m, insertWithi f' (l, k, x), r)
                     else bin (p, m, l, insertWithi f' (r, k, x))
                | (Bin (p, m, l, r), Tip (k, x)) =>
                     if not (matches (k, p, m))
                        then link (p, t1, k, t2)
                     else if (k andb m) = 0w0
                        then bin (p, m, insertWithi f (l, k, x), r)
                     else bin (p, m, l, insertWithi f (r, k, x))
                | (Bin (p, m, l, r), Bin (p', m', l', r')) =>
                     if m > m' (* `m > m'` means `m` is shorter than `m'` *)
                        then
                           if not (matches (p', p, m))
                              then link (p, t1, p', t2)
                           else if (p' andb m) = 0w0
                              then bin (p, m, go (l, t2), r)
                           else bin (p, m, l, go (r, t2))
                     else if m < m'
                        then
                           if not (matches (p, p', m'))
                              then link (p, t1, p', t2)
                           else if (p andb m') = 0w0
                              then bin (p', m', go (t1, l'), r')
                           else bin (p', m', l', go (t1, r'))
                     else if p = p'
                        then bin (p, m, go (l, l'), go (r, r'))
                     else link (p, t1, p', t2)
      in
         go (m1, m2)
      end

   fun unionWith f = unionWithi (fn (_, v, v') => f (v, v'))

   local
      fun f (_, v, _) = v
   in
      fun union (m1, m2) = unionWithi f (m1, m2)
   end

   fun mapi f t =
      let
         fun go t = 
            case DS.! t of
               Nil => t
             | Tip (k, x) =>
                  let
                     val x' = f (k, x)
                  in
                     if S.same (x, x')
                        then t
                     else DS.make (Tip (k, x'))
                  end
             | Bin (p, m, l, r) =>
                  let
                     val l' = go l
                     val r' = go r
                  in
                     (* Only need to check for pointer equality *)
                     if l = l' andalso r = r'
                        then t
                     else DS.make (Bin (p, m, l', r'))
                  end
      in
         go t
      end

   fun map f = mapi (fn (_, x) => f x)

   fun foldli f z t =
      let
         fun go (Nil, z) = z
           | go (Tip (k, x), z) = f (k, x, z)
           | go (Bin (_, _, l, r), z) = go (DS.! r, go (DS.! l, z))
      in
         go (DS.! t, z)
      end

   fun foldl f = foldli (fn (_, x, z) => f (x, z))

   fun foldri f z t =
      let
         fun go (Nil, z) = z
           | go (Tip (k, x), z) = f (k, x, z)
           | go (Bin (_, _, l, r), z) = go (DS.! l, go (DS.! r, z))
      in
         go (DS.! t, z)
      end

   fun foldr f = foldri (fn (_, x, z) => f (x, z))

   fun toList t = foldri (fn (k, v, l) => (k, v) :: l) nil t
end

structure M = LazySharingWordMapFn(
   struct
      type t = string
      val same: t * t -> bool = op =
   end)

(* vim: set tw=0 ts=3 sw=3: *)
