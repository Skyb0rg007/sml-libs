
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

         (* Hash the value
          * If two values are equal via `same`, they must have the same hash *)
         val hash: t -> word
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

and map = Map of {node: node DS.t, hash: word}

fun mask (k, m) = W.andb (k, W.xorb (m, W.+ (W.notb m, W.fromInt 1)))

fun hashCombine (w1, w2) =
   let
      open Word
      infix xorb >> <<
      val magic = 0wx9e3779b9
   in
      w1 xorb (w2 + magic + (w1 << 0w6) + (w1 >> 0w2))
   end

fun hash (Map {hash, ...}) = hash

fun node (Map {node, ...}) = DS.! node

fun node' (Map {node, ...}) = node

fun hashNode Nil = 0wxdeadbeef
  | hashNode (Tip (k, v)) = hashCombine (Word.fromLarge (W.toLarge k), Value.hash v)
  | hashNode (Bin (_, _, l, r)) = hashCombine (hash l, hash r)

fun make node = Map {node = DS.make node, hash = hashNode node}

fun link (p1, t1, p2, t2) =
   let
      val m = W.highestBitMask (W.xorb (p1, p2))
      val p = mask (p1, m)
   in
      if W.same (W.andb (p1, m), W.fromInt 0)
         then make (Bin (p, m, t1, t2))
      else make (Bin (p, m, t2, t1))
   end

fun nomatch (k, p, m) = not (W.same (mask (k, m), p))

fun zero (k, m) = W.same (W.andb (k, m), W.fromInt 0)

(* A mask is shorter if the value is larger, since we use big-endian *)
val shorter = W.>

val empty = make Nil

fun bin (p, m, l, r) =
   case node l of
      Nil => (DS.union (node' l, node' empty); r)
    | _ =>
         case node r of
            Nil => (DS.union (node' r, node' empty); l)
          | _ => make (Bin (p, m, l, r))

fun binCheckL (p, m, l, r) =
   case node l of
      Nil => (DS.union (node' l, node' empty); r)
    | _ => make (Bin (p, m, l, r))

fun binCheckR (p, m, l, r) =
   case node r of
      Nil => (DS.union (node' r, node' empty); l)
    | _ => make (Bin (p, m, l, r))

val tip = make o Tip

(** Exports **)

(* Determine if the two maps are the same
 * `O(n)` worst case, `O(α⁻¹(n))` amortized *)
fun equals (t1, t2) =
   if hash t1 <> hash t2
      then false
   else if DS.same (node' t1, node' t2)
      then true
   else if equalsNode (node t1, node t2)
      then (DS.union (node' t1, node' t2); true)
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

val singleton = tip

fun insertLookupWithi f (t, k, x) =
   let
      fun mapSnd f (a, b) = (a, f b)

      fun go t =
         case node t of
            Nil => (NONE, tip (k, x))
          | Tip (k', x') =>
               if W.same (k, k')
                  then (SOME x', tip (k, f (k, x', x)))
               else (NONE, link (k', t, k, tip (k, x)))
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then (NONE, link (p, t, k, tip (k, x)))
               else if zero (k, m)
                  then mapSnd (fn l' => make (Bin (p, m, l', r))) (go l)
               else mapSnd (fn r' => make (Bin (p, m, l, r'))) (go r)
   in
      go t
   end

fun insertWithi f (t, k, x) =
   let
      fun go t =
         case node t of
            Nil => tip (k, x)
          | Tip (k', x') =>
               if W.same (k, k')
                  then tip (k, f (k, x', x))
               else link (k', t, k, tip (k, x))
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then link (p, t, k, tip (k, x))
               else if zero (k, m)
                  then make (Bin (p, m, go l, r))
               else make (Bin (p, m, l, go r))
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
         case node t of
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
         case node t of
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
                        if DS.shallowEq (node' l, node' l')
                           then t
                        else make (Bin (p, m, l', r))
                     end
               else
                  let
                     val r' = go r
                  in
                     if DS.shallowEq (node' r, node' r')
                        then t
                     else make (Bin (p, m, l, r'))
                  end
   in
      go t
   end

fun update f (t, k) =
   let
      fun go t =
         case node t of
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
                        if DS.shallowEq (node' l, node' l')
                           then t
                        else binCheckL (p, m, l', r)
                     end
               else
                  let
                     val r' = go r
                  in
                     if DS.shallowEq (node' r, node' r')
                        then t
                     else binCheckR (p, m, l, r')
                  end
   in
      go t
   end

fun updateLookupWithi f (t, k) =
   let
      fun go t =
         case node t of
            Nil => (NONE, t)
          | Tip (k', x) =>
               if W.same (k, k')
                  then
                     case f (k, x) of
                        NONE => (SOME x, empty)
                      | SOME y =>
                           if Value.shallowEq (x, y)
                              then (SOME x, t)
                           else (SOME x, tip (k, y))
               else (NONE, t)
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then (NONE, t)
               else if zero (k, m)
                  then
                     let
                        val (ox, l') = go l
                     in
                        (* XXX: Check if `same (l, l')`? *)
                        (ox, binCheckL (p, m, l', r))
                     end
               else 
                  let
                     val (ox, r') = go r
                  in
                     (ox, binCheckR (p, m, l, r'))
                  end
   in
      go t
   end

fun alter f (t, k) =
   let
      fun go t =
         case node t of
            Nil => (case f NONE of
                       SOME x => tip (k, x)
                     | NONE => empty)
          | Tip (k', x) =>
               if W.same (k, k')
                  then
                     case f (SOME x) of
                        SOME y =>
                           if Value.shallowEq (x, y)
                              then t
                           else tip (k, y)
                      | NONE => empty
               else (case f NONE of
                        SOME y => link (k, tip (k, y), k', t)
                      | NONE => t)
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then
                     case f NONE of
                        SOME x => link (k, tip (k, x), p, t)
                      | NONE => t
               else if zero (k, m)
                  then
                     let
                        val l' = go l
                     in
                        bin (p, m, l', r)
                     end
               else
                  let
                     val r' = go r
                  in
                     bin (p, m, l, r')
                  end
   in
      go t
   end

(** Lookup **)

fun find (t, k) =
   let
      fun go t =
         case node t of
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
         case node t of
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
            case (node t1, node t2) of
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
                           then make (Bin (p1, m1, go (l1, t2), r1))
                        else make (Bin (p1, m1, l1, go (r1, t2)))
                  else if shorter (m2, m1)
                     then
                        if nomatch (p1, p2, m2)
                           then link (p1, t1, p2, t2)
                        else if zero (p1, m2)
                           then make (Bin (p2, m2, go (t1, l2), r2))
                        else make (Bin (p2, m2, l2, go (t1, r2)))
                  else if W.same (p1, p2)
                     then make (Bin (p1, m1, go (l1, l2), go (r1, r2)))
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
            case (node t1, node t2) of
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
                       then make (Bin (p1, m1, go (l1, t2), r1))
                    else make (Bin (p1, m1, l1, go (r1, t2)))
              else if shorter (m2, m1)
                 then
                    if nomatch (p1, p2, m2)
                       then t1
                    else if zero (p1, m2)
                       then go (t1, l2)
                    else go (t1, r2)
              else if W.same (p1, p2)
                 then make (Bin (p1, m1, go (l1, l2), go (r1, r2)))
              else t1
   in
      go
   end

fun differenceWith f = differenceWithi (fn (_, x, x') => f (x, x'))

fun difference (t1, t2) = differenceWithi (fn _ => NONE) (t1, t2)

(** Intersection **)

fun intersectionWithi f =
   let
      fun go (t1, t2) =
         if equals (t1, t2)
            then t1
         else
            case (node t1, node t2) of
               (Nil, _) => t1
             | (_, Nil) => t2
             | (Tip (k1, x1), _) =>
                  (case find (t2, k1) of
                      NONE => empty
                    | SOME y => tip (k1, y))
             | (_, Tip (k2, x2)) =>
                  (case find (t1, k2) of
                      NONE => empty
                    | SOME x1 =>
                        case f (k2, x1, x2) of
                           NONE => empty
                         | SOME y => tip (k2, y))
             | (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =>
                  if shorter (m1, m2)
                     then
                        if nomatch (p2, p1, m1)
                           then empty
                        else if zero (p2, m1)
                           then go (l1, t2)
                        else go (t1, t2)
                  else if shorter (m2, m1)
                     then
                        if nomatch (p1, p2, m2)
                           then empty
                        else if zero (p1, m2)
                           then go (t1, l2)
                        else go (t1, r2)
                  else if W.same (p1, p2)
                     then bin (p1, m1, go (l1, l2), go (r1, r2))
                  else empty
   in
      go
   end

fun intersectionWith f = intersectionWithi (fn (_, x, x') => f (x, x'))

fun intersection (t1, t2) = intersectionWithi (fn (_, x, _) => SOME x) (t1, t2)

fun disjoint (t1, t2) =
   if equals (t1, t2)
      then false
   else
      case (node t1, node t2) of
         (Nil, _) => true
       | (_, Nil) => true
       | (Tip (k1, _), _) => not (inDomain (t2, k1))
       | (_, Tip (k2, _)) => not (inDomain (t1, k2))
       | (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =>
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

fun mapi f t =
   case node t of
      Nil => t
    | Tip (k, x) =>
         let
            val y = f (k, x)
         in
            if Value.shallowEq (x, y)
               then t
            else tip (k, y)
         end
    | Bin (p, m, l, r) =>
         let
            val l' = mapi f l
            val r' = mapi f r
         in
            if equals (l, l') andalso equals (r, r')
               then t
            else make (Bin (p, m, l, r))
         end

fun map f = mapi (fn (_, x) => f x)

fun mapAccumLi f z t =
   let
      fun go (t, acc) =
         case node t of
            Nil => (acc, t)
          | Tip (k, x) =>
               let
                  val (acc', x') = f (k, x, acc)
               in
                  if Value.shallowEq (x, x')
                     then (acc', t)
                  else (acc', tip (k, x'))
               end
          | Bin (p, m, l, r) =>
               let
                  val (acc', l') = go (l, acc)
                  val (acc'', r') = go (r, acc')
               in
                  if equals (l, l') andalso equals (r, r')
                     then (acc'', t)
                  else (acc'', make (Bin (p, m, l, r)))
               end
   in
      go (t, z)
   end

fun mapAccumL f = mapAccumLi (fn (_, x, acc) => f (x, acc))

fun mapAccumRi f z t =
   let
      fun go (t, acc) =
         case node t of
            Nil => (acc, t)
          | Tip (k, x) =>
               let
                  val (acc', x') = f (k, x, acc)
               in
                  if Value.shallowEq (x, x')
                     then (acc', t)
                  else (acc', tip (k, x'))
               end
          | Bin (p, m, l, r) =>
               let
                  val (acc', l') = go (l, acc)
                  val (acc'', r') = go (r, acc')
               in
                  if equals (l, l') andalso equals (r, r')
                     then (acc'', t)
                  else (acc'', make (Bin (p, m, l, r)))
               end
   in
      go (t, z)
   end

fun mapAccumR f = mapAccumRi (fn (_, x, acc) => f (x, acc))

fun foldli f z t =
   let
      fun go (t, acc) =
         case node t of
            Nil => acc
          | Tip (k, x) => f (k, x, acc)
          | Bin (_, _, l, r) => go (r, go (l, acc))
   in
      go (t, z)
   end

fun foldl f = foldli (fn (_, x, acc) => f (x, acc))

fun foldri f z t =
   let
      fun go (t, acc) =
         case node t of
            Nil => acc
          | Tip (k, x) => f (k, x, acc)
          | Bin (_, _, l, r) => go (l, go (r, acc))
   in
      go (t, z)
   end

fun foldr f = foldri (fn (_, x, acc) => f (x, acc))

fun appi f t =
   case node t of
      Nil => ()
    | Tip (k, x) => f (k, x)
    | Bin (_, _, l, r) => (appi f l; appi f r)

fun app f = appi (fn (_, x) => f x)

fun existsi f t =
   case node t of
      Nil => false
    | Tip (k, x) => f (k, x)
    | Bin (_, _, l, r) => existsi f l orelse existsi f r

fun exists f = existsi (fn (_, x) => f x)

fun alli f t =
   case node t of
      Nil => true
    | Tip (k, x) => f (k, x)
    | Bin (_, _, l, r) => alli f l andalso alli f r

fun all f = alli (fn (_, x) => f x)

(** Conversion **)

fun keys t = foldri (fn (k, _, acc) => k :: acc) [] t

fun elems t = foldr op :: [] t

fun toList t = foldri (fn (k, x, acc) => (k, x) :: acc) [] t

datatype 'a seq_node = SNil | SCons of 'a * 'a seq
withtype 'a seq = unit -> 'a seq_node

fun toSeq t =
   let
      fun go (t, acc) () =
         case node t of
            Nil => acc ()
          | Tip (k, x) => SCons ((k, x), acc)
          | Bin (_, _, l, r) => go (l, go (r, acc)) ()
   in
      go (t, fn () => SNil)
   end

(** Filter **)

fun filteri p =
   let
      fun go t =
         case node t of
            Nil => t
          | Tip (k, x) =>
               if p (k, x)
                  then t
               else empty
          | Bin (p, m, l, r) =>
               let
                  val l' = go l
                  val r' = go r
               in
                  if equals (l, l') andalso equals (r, r')
                     then t
                  else bin (p, m, l', r')
               end
   in
      go
   end

fun filter p = filteri (fn (_, x) => p x)

fun mapPartiali f =
   let
      fun go t =
         case node t of
            Nil => t
          | Tip (k, x) => (case f (k, x) of
                              NONE => empty
                            | SOME y =>
                                 if Value.shallowEq (x, y)
                                    then t
                                 else tip (k, y))
          | Bin (p, m, l, r) =>
               let
                  val l' = go l
                  val r' = go r
               in
                  if equals (l, l') andalso equals (r, r')
                     then t
                  else bin (p, m, l', r')
               end
   in
      go
   end

fun mapPartial f = mapPartiali (fn (_, x) => f x)

fun mapEitheri f = raise Fail "mapEitheri: NYI"
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

fun partitioni p = raise Fail "partitioni: NYI"
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

fun isSubmap (t1, t2) = equals (empty, difference (t1, t2))

fun isSubmapBy _ = raise Fail "isSubmapBy: NYI"
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

fun isProperSubmapBy pred (t1, t2) = raise Fail "isProperSubmapBy: NYI"
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

fun liftEquals eq (t1, t2) =
   if equals (t1, t2)
      then true
   else
      case (node t1, node t2) of
         (Nil, Nil) => true
       | (Tip (k, x), Tip (k', x')) =>
            W.same (k, k') andalso eq (x, x')
       | (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =>
            W.same (m1, m2)
            andalso W.same (p1, p2)
            andalso liftEquals eq (l1, l2)
            andalso liftEquals eq (r1, r2)
       | _ => false

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

fun viewMin t =
   if equals (t, empty)
      then NONE
   else
      let
         fun go t =
            case node t of
               Nil => raise Fail "LSSWordMapFn.viewMin: invalid Nil"
             | Tip (k, x) => (k, x, empty)
             | Bin (p, m, l, r) =>
                  case go l of
                     (k, x, l') => (k, x, binCheckL (p, m, l', r))
      in
         SOME (go t)
      end

fun viewMax t =
   if equals (t, empty)
      then NONE
   else
      let
         fun go t =
            case node t of
               Nil => raise Fail "LSSWordMapFn.viewMax: invalid Nil"
             | Tip (k, x) => (k, x, empty)
             | Bin (p, m, l, r) =>
                  case go r of
                     (k, x, r') => (k, x, binCheckR (p, m, l, r'))
      in
         SOME (go t)
      end

fun findMin t = Option.map (fn (k, x, _) => (k, x)) (viewMin t)
fun findMax t = Option.map (fn (k, x, _) => (k, x)) (viewMax t)
fun deleteMin t = Option.map #3 (viewMin t)
fun deleteMax t = Option.map #3 (viewMax t)

end

(* vim: set tw=0 ts=3 sw=3: *)
