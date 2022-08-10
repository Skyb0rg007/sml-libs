
signature LSS_WORD_MAP_STRUCTS =
sig
   structure W:
      sig
         include WORD

         val same: word * word -> bool
         val highestBitMask: word -> word
      end

   structure Value:
      sig
         type t

         (* Do these two values represent the same value? *)
         val same: t * t -> bool

         (* If this returns true, the values are the same
          * If this returns false, nothing is known about the values
          * Used for optimization *)
         val shallowEq: t * t -> bool
      end
end

functor LSSWordMapFn(S: LSS_WORD_MAP_STRUCTS): LSS_WORD_MAP =
struct

open S

structure DS = DisjointSet

type key = W.word
type value = Value.t

datatype node =
   Nil
 | Tip of key * value
 | Bin of W.word * W.word * map * map

withtype map = node DS.t

fun mask (k, m) = W.andb (k, W.xorb (m, W.+ (W.notb m, W.fromInt 1)))

fun link (p1, t1, p2, t2) =
   let
      val m = W.highestBitMask (W.xorb (p1, p2))
      val p = mask (p1, m)
   in
      if W.same (W.andb (p1, m), W.fromInt 0)
         then DS.make (Bin (p, m, t1, t2))
      else DS.make (Bin (p, m, t2, t1))
   end

fun nomatch (k, p, m) = not (W.same (mask (k, m), p))

fun zero (k, m) = W.same (W.andb (k, m), W.fromInt 0)

(* A mask is shorter if the value is larger, since we use big-endian *)
val shorter = W.>

fun bin (p, m, l, r) =
   case DS.! l of
      Nil => r
    | _ =>
         case DS.! r of
            Nil => l
          | _ => DS.make (Bin (p, m, l, r))

fun binCheckL (p, m, l, r) =
   case DS.! l of
      Nil => r
    | _ => DS.make (Bin (p, m, l, r))

fun binCheckR (p, m, l, r) =
   case DS.! r of
      Nil => l
    | _ => DS.make (Bin (p, m, l, r))

val tip = DS.make o Tip

(** Exports **)

(* Determine if the two maps are the same
 * `O(n)` worst case, `O(α⁻¹(n))` amortized *)
fun equals (t1, t2) =
   if DS.same (t1, t2)
      then true
   else if equalsNode (DS.! t1, DS.! t2)
      then (DS.union (t1, t2); true)
   else false

and equalsNode (Nil, Nil) = true
  | equalsNode (Tip (k1, x1), Tip (k2, x2)) =
   W.same (k1, k2)
   andalso Value.same (x1, x2)
  | equalsNode (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =
   W.same (m1, m2)
   andalso W.same (p1, p2)
   andalso equals (l1, l2)
   andalso equals (r1, r2)
  | equalsNode _ = false

val empty = DS.make Nil

val singleton = tip

fun insertLookupWithi f (t, k, x) =
   let
      fun mapSnd f (a, b) = (a, f b)

      fun go t =
         case DS.! t of
            Nil => (NONE, tip (k, x))
          | Tip (k', x') =>
               if W.same (k, k')
                  then (SOME x', tip (k, f (k, x', x)))
               else (NONE, link (k', t, k, tip (k, x)))
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then (NONE, link (p, t, k, tip (k, x)))
               else if zero (k, m)
                  then mapSnd (fn l' => DS.make (Bin (p, m, l', r))) (go l)
               else mapSnd (fn r' => DS.make (Bin (p, m, l, r'))) (go r)
   in
      go t
   end

fun insertWithi f (t, k, x) =
   let
      fun go t =
         case DS.! t of
            Nil => tip (k, x)
          | Tip (k', x') =>
               if W.same (k, k')
                  then tip (k, f (k, x', x))
               else link (k', t, k, tip (k, x))
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then link (p, t, k, tip (k, x))
               else if zero (k, m)
                  then DS.make (Bin (p, m, go l, r))
               else DS.make (Bin (p, m, l, go r))
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
      fun go t =
         case DS.! t of
            Nil => NONE
          | Tip (k', x') =>
               if W.same (k, k')
                  then SOME (empty, x')
               else NONE
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then NONE
               else if zero (k, m)
                  then 
                     case go l of
                        NONE => NONE
                      | SOME (l', x) => SOME (binCheckL (p, m, l', r), x)
               else
                  case go r of
                     NONE => NONE
                   | SOME (r', x) => SOME (binCheckR (p, m, l, r'), x)
   in
      go t
   end

fun delete (t, k) =
   case remove (t, k) of
      NONE => t
    | SOME (t', _) => t'

fun adjust f (t, k) =
   let
      fun go t =
         case DS.! t of
            Nil => t
          | Tip (k', x') =>
               if W.same (k, k')
                  then
                     let
                        val y = f x'
                     in
                        if Value.shallowEq (x', y)
                           then t
                        else tip (k, y)
                     end
               else t
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then t
               else if zero (k, m)
                  then
                     let
                        val l' = go l
                     in
                        if DS.shallowEq (l, l')
                           then t
                        else DS.make (Bin (p, m, l', r))
                     end
               else
                  let
                     val r' = go r
                  in
                     if DS.shallowEq (r, r')
                        then t
                     else DS.make (Bin (p, m, l, r'))
                  end
   in
      go t
   end

fun update f (t, k) =
   let
      fun go t =
         case DS.! t of
            Nil => t
          | Tip (k', x') =>
               if W.same (k, k')
                  then
                     case f x' of
                        NONE => empty
                      | SOME y =>
                           if Value.shallowEq (x', y)
                              then t
                           else tip (k, y)
               else t
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then t
               else if zero (k, m)
                  then
                     let
                        val l' = go l
                     in
                        if DS.shallowEq (l, l')
                           then t
                        else binCheckL (p, m, l', r)
                     end
               else
                  let
                     val r' = go r
                  in
                     if DS.shallowEq (r, r')
                        then t
                     else binCheckR (p, m, l, r')
                  end
   in
      go t
   end

fun updateLookupWithi f (t, k) = raise Fail "NYI"
   (* let *)
   (*    fun go Nil = (NONE, Nil) *)
   (*      | go (t as Tip (k', x')) = *)
   (*       if W.same (k, k') *)
   (*          then *)
   (*             case f (k, x') of *)
   (*                NONE => (SOME x', Nil) *)
   (*              | SOME y => (SOME x', Tip (k, y)) *)
   (*       else (NONE, t) *)
   (*      | go (t as Bin (p, m, l, r)) = *)
   (*       if nomatch (k, p, m) *)
   (*          then (NONE, t) *)
   (*       else if zero (k, m) *)
   (*          then *) 
   (*             case go l of *)
   (*                (ox, Nil) => (ox, r) *)
   (*              | (ox, l') => (ox, Bin (p, m, l', r)) *)
   (*       else *) 
   (*          case go r of *)
   (*             (ox, Nil) => (ox, l) *)
   (*           | (ox, r') => (ox, Bin (p, m, l, r')) *)
   (* in *)
   (*    go t *)
   (* end *)

fun alter f (t, k) = raise Fail "NYI"
   (* let *)
   (*    fun go Nil = *)
   (*       (case f NONE of *)
   (*          SOME x => Tip (k, x) *)
   (*        | NONE => Nil) *)
   (*      | go (t as Tip (k', x')) = *)
   (*       if W.same (k, k') *)
   (*          then *)
   (*             case f (SOME x') of *)
   (*                SOME y => Tip (k, y) *)
   (*              | NONE => Nil *)
   (*       else *)
   (*          (case f NONE of *)
   (*             SOME y => link (k, Tip (k, y), k', t) *)
   (*           | NONE => t) *)
   (*      | go (t as Bin (p, m, l, r)) = *)
   (*       if nomatch (k, p, m) *)
   (*          then *)
   (*             case f NONE of *)
   (*                SOME x => link (k, Tip (k, x), p, t) *)
   (*              | NONE => t *)
   (*       else if zero (k, m) *)
   (*          then bin (p, m, go l, r) *)
   (*       else bin (p, m, l, go r) *)
   (* in *)
   (*    go t *)
   (* end *)

(** Lookup **)

fun find (t, k) =
   let
      fun go t =
         case DS.! t of
            Nil => NONE
          | Tip (k', x') =>
               if W.same (k, k')
                  then SOME x'
               else NONE
          | Bin (p, m, l, r) =>
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

fun isEmpty t = equals (t, empty)

fun size t =
   let
      fun go (t, acc) =
         case DS.! t of
            Nil => acc
          | Tip _ => acc + 1
          | Bin (_, _, l, r) => go (l, go (r, acc))
   in
      go (t, 0)
   end

(** Union **)

fun unionWithi f =
   let
      fun f' (k, x2, x1) = f (k, x1, x2)

      fun go (t1, t2) =
         if equals (t1, t2)
            then t1
         else
            case (DS.! t1, DS.! t2) of
               (Nil, _) => t2
             | (_, Nil) => t1
             | (Tip (k1, x1), _) => insertWithi f' (t2, k1, x1)
             | (_, Tip (k2, x2)) => insertWithi f (t1, k2, x2)
             | (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =>
                  if shorter (m1, m2)
                     then
                        if nomatch (p2, p1, m1)
                           then link (p1, t1, p2, t2)
                        else if zero (p2, m1)
                           then DS.make (Bin (p1, m1, go (l1, t2), r1))
                        else DS.make (Bin (p1, m1, l1, go (r1, t2)))
                  else if shorter (m2, m1)
                     then
                        if nomatch (p1, p2, m2)
                           then link (p1, t1, p2, t2)
                        else if zero (p1, m2)
                           then DS.make (Bin (p2, m2, go (t1, l2), r2))
                        else DS.make (Bin (p2, m2, l2, go (t1, r2)))
                  else if W.same (p1, p2)
                     then DS.make (Bin (p1, m1, go (l1, l2), go (r1, r2)))
                  else link (p1, t1, p2, t2)
   in
      go
   end

fun unionWith f = unionWithi (fn (_, x, x') => f (x, x'))

(** Difference **)

fun differenceWithi f =
   let
      fun go (t1, t2) =
         if equals (t1, t2)
            then empty
         else
            case (DS.! t1, DS.! t2) of
               (Nil, _) => t1
             | (_, Nil) => t1
             | (Tip (k1, x1), _) =>
              (case find (t2, k1) of
                 NONE => t1
               | SOME x2 =>
                    case f (k1, x1, x2) of
                       NONE => empty
                     | SOME x =>
                           if Value.shallowEq (x, x2)
                              then t1
                           else tip (k1, x))
             | (_, Tip (k2, x2)) => update (fn x1 => f (k2, x1, x2)) (t1, k2)
             | (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =>
              if shorter (m1, m2)
                 then
                    if nomatch (p2, p1, m1)
                       then t1
                    else if zero (p2, m1)
                       then DS.make (Bin (p1, m1, go (l1, t2), r1))
                    else DS.make (Bin (p1, m1, l1, go (r1, t2)))
              else if shorter (m2, m1)
                 then
                    if nomatch (p1, p2, m2)
                       then t1
                    else if zero (p1, m2)
                       then go (t1, l2)
                    else go (t1, r2)
              else if W.same (p1, p2)
                 then DS.make (Bin (p1, m1, go (l1, l2), go (r1, r2)))
              else t1
   in
      go
   end

fun differenceWith f = differenceWithi (fn (_, x, x') => f (x, x'))

fun difference (t1, t2) = differenceWithi (fn _ => NONE) (t1, t2)

(** Intersection **)

fun intersectionWithi f = raise Fail "NYI"
   (* let *)
   (*    fun go (Nil, _) = Nil *)
   (*      | go (_, Nil) = Nil *)
   (*      | go (Tip (k1, x1), t2) = *)
   (*       (case find (t2, k1) of *)
   (*          NONE => Nil *)
   (*        | SOME x2 => *)
   (*             case f (k1, x1, x2) of *)
   (*                NONE => Nil *)
   (*              | SOME y => Tip (k1, y)) *)
   (*      | go (t1, Tip (k2, x2)) = *)
   (*       (case find (t1, k2) of *)
   (*          NONE => Nil *)
   (*        | SOME x1 => *)
   (*             case f (k2, x1, x2) of *)
   (*                NONE => Nil *)
   (*              | SOME y => Tip (k2, y)) *)
   (*      | go (t1 as Bin (p1, m1, l1, r1), t2 as Bin (p2, m2, l2, r2)) = *)
   (*       if shorter (m1, m2) *)
   (*          then *)
   (*             if nomatch (p2, p1, m1) *)
   (*                then Nil *)
   (*             else if zero (p2, m1) *)
   (*                then go (l1, t2) *)
   (*             else go (r1, t2) *)
   (*       else if shorter (m2, m1) *)
   (*          then *)
   (*             if nomatch (p1, p2, m2) *)
   (*                then Nil *)
   (*             else if zero (p1, m2) *)
   (*                then go (t1, l2) *)
   (*             else go (t1, r2) *)
   (*       else if W.same (p1, p2) *)
   (*          then bin (p1, m1, go (l1, l2), go (r1, r2)) *)
   (*       else Nil *)
   (* in *)
   (*    go *)
   (* end *)

fun intersectionWith f = intersectionWithi (fn (_, x, x') => f (x, x'))

fun intersection (t1, t2) = intersectionWithi (fn (_, x, _) => SOME x) (t1, t2)

fun disjoint _ = raise Fail "NYI"
(* fun disjoint (Nil, _) = true *)
(*   | disjoint (_, Nil) = true *)
(*   | disjoint (Tip (k1, _), t2) = not (inDomain (t2, k1)) *)
(*   | disjoint (t1, Tip (k2, _)) = not (inDomain (t1, k2)) *)
(*   | disjoint (t1 as Bin (p1, m1, l1, r1), t2 as Bin (p2, m2, l2, r2)) = *)
(*    if shorter (m1, m2) *)
(*       then *)
(*          if nomatch (p2, p1, m1) *)
(*             then true *)
(*          else if zero (p2, m1) *)
(*             then disjoint (l1, t2) *)
(*          else disjoint (r1, t2) *)
(*    else if shorter (m2, m1) *)
(*       then *)
(*          if nomatch (p1, p2, m2) *)
(*             then true *)
(*          else if zero (p1, m2) *)
(*             then disjoint (t1, l2) *)
(*          else disjoint (t1, r2) *)
(*    else if W.same (p1, p2) *)
(*       then disjoint (l1, l2) andalso disjoint (r1, r2) *)
(*    else true *)

(** Traversal **)

fun mapi _ = raise Fail "NYI"
(* fun mapi _ Nil = Nil *)
(*   | mapi f (Tip (k, x)) = Tip (k, f (k, x)) *)
(*   | mapi f (Bin (p, m, l, r)) = Bin (p, m, mapi f l, mapi f r) *)

fun map f = mapi (fn (_, x) => f x)

fun mapAccumLi f z t = raise Fail "NYI"
   (* let *)
   (*    fun go (Nil, acc) = (acc, Nil) *)
   (*      | go (Tip (k, x), acc) = *)
   (*       let *)
   (*          val (acc, x) = f (k, x, acc) *)
   (*       in *)
   (*          (acc, Tip (k, x)) *)
   (*       end *)
   (*      | go (Bin (p, m, l, r), acc) = *)
   (*       let *)
   (*          val (acc, l) = go (l, acc) *)
   (*          val (acc, r) = go (r, acc) *)
   (*       in *)
   (*          (acc, Bin (p, m, l, r)) *)
   (*       end *)
   (* in *)
   (*    go (t, z) *)
   (* end *)

fun mapAccumL f = mapAccumLi (fn (_, x, acc) => f (x, acc))

fun mapAccumRi f z t = raise Fail "NYI"
   (* let *)
   (*    fun go (Nil, acc) = (acc, Nil) *)
   (*      | go (Tip (k, x), acc) = *)
   (*       let *)
   (*          val (acc, x) = f (k, x, acc) *)
   (*       in *)
   (*          (acc, Tip (k, x)) *)
   (*       end *)
   (*      | go (Bin (p, m, l, r), acc) = *)
   (*       let *)
   (*          val (acc, r) = go (r, acc) *)
   (*          val (acc, l) = go (l, acc) *)
   (*       in *)
   (*          (acc, Bin (p, m, l, r)) *)
   (*       end *)
   (* in *)
   (*    go (t, z) *)
   (* end *)

fun mapAccumR f = mapAccumRi (fn (_, x, acc) => f (x, acc))

fun foldli f z t = raise Fail "NYI"
   (* let *)
   (*    fun go (Nil, acc) = acc *)
   (*      | go (Tip (k, x), acc) = f (k, x, acc) *)
   (*      | go (Bin (_, _, l, r), acc) = go (r, go (l, acc)) *)
   (* in *)
   (*    go (t, z) *)
   (* end *)

fun foldl f = foldli (fn (_, x, acc) => f (x, acc))

fun foldri f z t = raise Fail "NYI"
   (* let *)
   (*    fun go (Nil, acc) = acc *)
   (*      | go (Tip (k, x), acc) = f (k, x, acc) *)
   (*      | go (Bin (_, _, l, r), acc) = go (l, go (r, acc)) *)
   (* in *)
   (*    go (t, z) *)
   (* end *)

fun foldr f = foldri (fn (_, x, acc) => f (x, acc))

fun appi _ = raise Fail "NYI"
(* fun appi _ Nil = () *)
(*   | appi f (Tip (k, x)) = f (k, x) *)
(*   | appi f (Bin (_, _, l, r)) = (appi f l; appi f r) *)

fun app f = appi (fn (_, x) => f x)

fun existsi _ = raise Fail "NYI"
(* fun existsi _ Nil = false *)
(*   | existsi f (Tip (k, x)) = f (k, x) *)
(*   | existsi f (Bin (_, _, l, r)) = existsi f l orelse existsi f r *)

fun exists f = existsi (fn (_, x) => f x)

fun alli _ = raise Fail "NYI"
(* fun alli _ Nil = true *)
(*   | alli f (Tip (k, x)) = f (k, x) *)
(*   | alli f (Bin (_, _, l, r)) = alli f l andalso alli f r *)

fun all f = alli (fn (_, x) => f x)

(** Conversion **)

fun keys t = foldri (fn (k, _, acc) => k :: acc) [] t

fun elems t = foldr op :: [] t

fun toList t = foldri (fn (k, x, acc) => (k, x) :: acc) [] t

(** Filter **)

fun filteri p = raise Fail "NYI"
   (* let *)
   (*    fun go Nil = Nil *)
   (*      | go (t as Tip (k, x)) = *)
   (*       if p (k, x) *)
   (*          then t *)
   (*       else Nil *)
   (*      | go (Bin (p, m, l, r)) = bin (p, m, go l, go r) *)
   (* in *)
   (*    go *)
   (* end *)

fun filter p = filteri (fn (_, x) => p x)

fun mapPartiali f = raise Fail "NYI"
   (* let *)
   (*    fun go Nil = Nil *)
   (*      | go (Tip (k, x)) = *)
   (*       (case f (k, x) of *)
   (*          NONE => Nil *)
   (*        | SOME y => Tip (k, y)) *)
   (*      | go (Bin (p, m, l, r)) = bin (p, m, go l, go r) *)
   (* in *)
   (*    go *)
   (* end *)

fun mapPartial f = mapPartiali (fn (_, x) => f x)

fun mapEitheri f = raise Fail "NYI"
   (* let *)
   (*    fun go Nil = (Nil, Nil) *)
   (*      | go (Tip (k, x)) = *)
   (*       (case f (k, x) of *)
   (*          Either.INL y => (Tip (k, y), Nil) *)
   (*        | Either.INR z => (Nil, Tip (k, z))) *)
   (*      | go (Bin (p, m, l, r)) = *)
   (*       let *)
   (*          val (l1, l2) = go l *)
   (*          val (r1, r2) = go r *)
   (*       in *)
   (*          (bin (p, m, l1, r1), bin (p, m, l2, r2)) *)
   (*       end *)
   (* in *)
   (*    go *)
   (* end *)

fun mapEither f = mapEitheri (fn (_, x) => f x)

fun partitioni p = raise Fail "NYI"
   (* let *)
   (*    fun go Nil = (Nil, Nil) *)
   (*      | go (t as Tip (k, x)) = *)
   (*       if p (k, x) *)
   (*          then (t, Nil) *)
   (*       else (Nil, t) *)
   (*      | go (Bin (p, m, l, r)) = *)
   (*       let *)
   (*          val (l1, l2) = go l *)
   (*          val (r1, r2) = go r *)
   (*       in *)
   (*          (bin (p, m, l1, r1), bin (p, m, l2, r2)) *)
   (*       end *)
   (* in *)
   (*    go *)
   (* end *)

fun partition p = partitioni (fn (_, x) => p x)

(** Submap **)

fun isSubmapBy _ = raise Fail "NYI"
(* fun isSubmapBy _ (Nil, _) = true *)
(*   | isSubmapBy _ (_, Nil) = false *)
(*   | isSubmapBy pred (Tip (k, x), t) = *)
(*    (case find (t, k) of *)
(*       NONE => false *)
(*     | SOME y => pred (x, y)) *)
(*   | isSubmapBy _ (Bin _, Tip _) = false *)
(*   | isSubmapBy pred (t1 as Bin (p1, m1, l1, r1), t2 as Bin (p2, m2, l2, r2)) = *)
(*    if shorter (m1, m2) *)
(*       then false *)
(*    else if shorter (m2, m1) *)
(*       then *)
(*          if nomatch (p1, p2, m2) *)
(*             then false *)
(*          else if zero (p1, m2) *)
(*             then isSubmapBy pred (t1, l2) *)
(*          else isSubmapBy pred (t1, r2) *)
(*    else *)
(*       W.same (p1, p2) *)
(*       andalso isSubmapBy pred (l1, l2) *)
(*       andalso isSubmapBy pred (r1, r2) *)

fun isProperSubmapBy pred (t1, t2) = raise Fail "NYI"
   (* let *)
   (*    (1* LESS -> proper submap *)
   (*     * EQUAL -> equal *)
   (*     * GREATER -> not a submap (disjoint) *1) *)
   (*    fun go (Nil, Nil) = EQUAL *)
   (*      | go (Nil, _) = LESS *)
   (*      | go (Tip (k, x), Tip (k', x')) = *)
   (*       if W.same (k, k') andalso pred (x, x') *)
   (*          then EQUAL *)
   (*       else GREATER *)
   (*      | go (Tip (k, x), t) = *)
   (*       (case find (t, k) of *)
   (*          NONE => GREATER *)
   (*        | SOME y => if pred (x, y) then LESS else GREATER) *)
   (*      | go (t1 as Bin (p1, m1, l1, r1), t2 as Bin (p2, m2, l2, r2)) = *)
   (*       if shorter (m1, m2) *)
   (*          then GREATER *)
   (*       else if shorter (m2, m1) *)
   (*          then *)
   (*             if nomatch (p1, p2, m2) *)
   (*                then GREATER *)
   (*             else if zero (p1, m2) *)
   (*                then go (t1, l2) *)
   (*             else go (t1, r2) *)
   (*       else if W.same (p1, m2) *)
   (*          then *)
   (*             case go (l1, l2) of *)
   (*                GREATER => GREATER *)
   (*              | lcmp => *)
   (*                   case (lcmp, go (r1, r2)) of *)
   (*                      (_, GREATER) => GREATER *)
   (*                    | (EQUAL, EQUAL) => EQUAL *)
   (*                    | _ => LESS *)
   (*       else GREATER *)
   (*      | go (Bin _, _) = GREATER *)
   (* in *)
   (*    go (t1, t2) = LESS *)
   (* end *)

fun liftEquals _ = raise Fail "NYI"
(* fun liftEquals _ (Nil, Nil) = true *)
(*   | liftEquals eq (Tip (k, x), Tip (k', x')) = *)
(*    W.same (k, k') *)
(*    andalso eq (x, x') *)
(*   | liftEquals eq (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) = *)
(*    W.same (m1, m2) *)
(*    andalso W.same (p1, p2) *)
(*    andalso liftEquals eq (l1, l2) *)
(*    andalso liftEquals eq (r1, r2) *)
(*   | liftEquals _ (_, _) = false *)

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

fun viewMin _: (key * value * map) option = raise Fail "NYI"
fun viewMax _: (key * value * map) option = raise Fail "NYI"
(* fun viewMin Nil = NONE *)
(*   | viewMin t = *)
(*    let *)
(*       fun go Nil = raise Fail "WordMapFn.viewMin: invalid Nil" *)
(*         | go (Tip (k, x)) = (k, x, Nil) *)
(*         | go (Bin (p, m, l, r)) = *)
(*          case go l of *)
(*             (k, x, l') => (k, x, binCheckL (p, m, l', r)) *)
(*    in *)
(*       SOME (go t) *)
(*    end *)

(* fun viewMax Nil = NONE *)
(*   | viewMax t = *)
(*    let *)
(*       fun go Nil = raise Fail "WordMapFn.viewMax: invalid Nil" *)
(*         | go (Tip (k, x)) = (k, x, Nil) *)
(*         | go (Bin (p, m, l, r)) = *)
(*          case go r of *)
(*               (k, x, r') => (k, x, binCheckR (p, m, l, r')) *)
(*    in *)
(*       SOME (go t) *)
(*    end *)

fun findMin t = Option.map (fn (k, x, _) => (k, x)) (viewMin t)
fun findMax t = Option.map (fn (k, x, _) => (k, x)) (viewMax t)
fun deleteMin t = Option.map #3 (viewMin t)
fun deleteMax t = Option.map #3 (viewMax t)

end

(* vim: set tw=0 ts=3 sw=3: *)
