
functor LSSWordMapFn(S: LSS_WORD_MAP_STRUCTS): LSS_WORD_MAP =
struct

open S

type key = word
type value = Value.t
type augment = Aug.t

datatype node =
   Nil
 | Tip of key * value
 | Bin of (* prefix *) word * (* mask *) word * map * map

and map = Map of node DisjointSet.t * augment

(* Accessors *)

fun augment (Map (_, a)) = a

fun nodeRef (Map (r, _)) = r

fun node m = DisjointSet.! (nodeRef m)

(* Smart constructors *)

val empty =
   Map (DisjointSet.make Nil, Aug.zero)

fun tip (k, x) =
   Map (DisjointSet.make (Tip (k, x)), Aug.pure (k, x))

fun bin (p, m, l, r) =
   Map (DisjointSet.make (Bin (p, m, l, r)), Aug.+ (augment l, augment r))

(* No need to perform union, since all `Nil`s are made via `empty` *)
fun isEmpty t =
   case node t of
      Nil => true
    | _ => false

fun binCheck (p, m, l, r) =
   if isEmpty l
      then r
   else if isEmpty r
      then l
   else bin (p, m, l, r)

fun binCheckL (p, m, l, r) =
   if isEmpty l
      then r
   else bin (p, m, l, r)

fun binCheckR (p, m, l, r) =
   if isEmpty r
      then l
   else bin (p, m, l, r)

(* Bitmask helpers *)

(* The prefix of `k` from the highest bit to the set bit in `m` *)
fun mask (k, m) =
   Word.andb (k, Word.xorb (m, Word.notb m + 0w1))

(* Does the key `k` match the prefix `p` and mask `m`? *)
fun nomatch (k, p, m) = mask (k, m) <> p

(* Returns whether the key `k` has the `m`-bit set *)
fun zero (k, m) = Word.andb (k, m) = 0w0

(* A bitmask is shorter when the value is larger, since it's big-endian *)
val shorter = Word.>

(* Build a node from subtrees `t1` and `t2` with prefixes `p1` and `p2`
 * Precondition: `p1` and `p2` are not equal
 * Ie. the subtrees do not share any nodes in common
 *)
fun link (p1, t1, p2, t2) =
   let
      val m = WordEx.highestBitMask (Word.xorb (p1, p2))
      val p = mask (p1, m)
   in
      if Word.andb (p1, m) = Word.xorb (p1, p2)
         then bin (p, m, t1, t2)
      else bin (p, m, t2, t1)
   end

(* Exports *)
fun shallowEq (t1, t2) = DisjointSet.equals (nodeRef t1, nodeRef t2)

fun equals (m1, m2) =
   let
      fun go (t1, t2) =
         if DisjointSet.equals (nodeRef t1, nodeRef t2)
            then true
         else if not (Aug.equals (augment t1, augment t2))
            then false
         else if goNode (node t1, node t2)
            then (DisjointSet.union (nodeRef t1, nodeRef t2); true)
         else false

      and goNode (Nil, Nil) = true
        | goNode (Tip (kx, x), Tip (ky, y)) =
         kx = ky
         andalso Value.equals (x, y)
        | goNode (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =
         m1 = m2
         andalso p1 = p2
         andalso go (l1, l2)
         andalso go (r1, r2)
        | goNode _ = false
   in
      go (m1, m2)
   end

val singleton = tip

fun insertLookupWithi f (t, kx, x) =
   let
      val new = tip (kx, x)

      fun go t =
         case node t of
            Nil => (NONE, new)
          | Tip (ky, y) =>
               if kx = ky
                  then (SOME y, new)
               else (NONE, link (ky, t, kx, new))
          | Bin (p, m, l, r) =>
               if nomatch (kx, p, m)
                  then (NONE, link (p, t, kx, new))
               else if zero (kx, m)
                  then case go l of (old, l') => (old, bin (p, m, l', r))
               else case go r of (old, r') => (old, bin (p, m, l, r'))
   in
      go t
   end

fun insertWithi f (t, kx, x) =
   let
      val new = tip (kx, x)

      fun go t =
         case node t of
            Nil => new
          | Tip (ky, y) =>
               if kx = ky
                  then new
               else link (ky, t, kx, new)
          | Bin (p, m, l, r) =>
               if nomatch (kx, p, m)
                  then link (p, t, kx, new)
               else if zero (kx, m)
                  then bin (p, m, go l, r)
               else bin (p, m, l, go r)
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
          | Tip (kx, x) =>
               if kx = k
                  then SOME (empty, x)
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
      SOME (t', _) => t'
    | NONE => t

fun adjust f (t, k) =
   let
      fun go t =
         case node t of
            Nil => empty
          | Tip (kx, x) =>
               if kx = k
                  then tip (k, f x)
               else t
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then t
               else if zero (k, m)
                  then bin (p, m, go l, r)
               else bin (p, m, l, go r)
   in
      go t
   end

fun update f (t, k) =
   let
      fun go t =
         case node t of
            Nil => t
          | Tip (kx, x) =>
               if kx = k
                  then
                     case f x of
                        NONE => empty
                      | SOME y => tip (k, y)
               else t
          | Bin (p, m, l, r) =>
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
      fun go t =
         case node t of
            Nil => (NONE, t)
          | Tip (kx, x) =>
               if kx = k
                  then
                     case f (k, x) of
                        NONE => (SOME x, empty)
                      | SOME y => (SOME x, tip (k, y))
               else (NONE, t)
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then (NONE, t)
               else if zero (k, m)
                  then case go l of (old, l') => (old, binCheckL (p, m, l', r))
               else case go r of (old, r') => (old, binCheckR (p, m, l, r'))
   in
      go t
   end

fun alter f (t, k) =
   let
      fun notFound () =
         case f NONE of
            NONE => empty
          | SOME y => tip (k, y)

      fun notFoundLink (p, t) =
         case f NONE of
            NONE => t
          | SOME y => link (k, tip (k, y), p, t)

      fun found x =
         case f (SOME x) of
            NONE => empty
          | SOME y => tip (k, y)

      fun go t =
         case node t of
            Nil => notFound ()
          | Tip (kx, x) =>
               if kx = k
                  then found x
               else notFoundLink (kx, t)
          | Bin (p, m, l, r) =>
               if nomatch (k, p, m)
                  then notFoundLink (p, t)
               else if zero (k, m)
                  then bin (p, m, go l, r)
               else bin (p, m, l, go r)
   in
      go t
   end

(** Lookup **)

fun find (t, k) =
   let
      fun go t =
         case node t of
            Nil => NONE
          | Tip (kx, x) =>
               if kx = k
                  then SOME x
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
                        (* t1 is possibly a subset since it has a shorter prefix *)
                        if nomatch (p2, p1, m1)
                           (* Subtrees are disjoint *)
                           then link (p1, t1, p2, t2)
                        else if zero (p2, m1)
                           (* t1 is subset of t2, overlapping on left *)
                           then bin (p1, m1, go (l1, t2), r1)
                           (* t1 is subset of t2, overlapping on right *)
                        else bin (p1, m1, l1, go (r1, t2))
                  else if shorter (m2, m1)
                     then
                        if nomatch (p1, p2, m2)
                           then link (p1, t1, p2, t2)
                        else if zero (p1, m2)
                           then bin (p2, m2, go (t1, l2), r2)
                        else bin (p2, m2, l2, go (t1, r2))
                  else if p1 = p2
                     then bin (p1, m1, go (l1, l2), go (r1, r2))
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
                     | SOME x => tip (k1, x))
             | (_, Tip (k2, x2)) => update (fn x1 => f (k2, x1, x2)) (t1, k2)
             | (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =>
              if shorter (m1, m2)
                 then
                    if nomatch (p2, p1, m1)
                       then t1
                    else if zero (p2, m1)
                       then bin (p1, m1, go (l1, t2), r1)
                    else bin (p1, m1, l1, go (r1, t2))
              else if shorter (m2, m1)
                 then
                    if nomatch (p1, p2, m2)
                       then t1
                    else if zero (p1, m2)
                       then go (t1, l2)
                    else go (t1, r2)
              else if p1 = p2
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
                  else if p1 = p2
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
            else if p1 = p2
               then disjoint (l1, l2) andalso disjoint (r1, r2)
            else true

(** Traversal **)

fun mapi f t =
   case node t of
      Nil => t
    | Tip (k, x) => tip (k, f (k, x))
    | Bin (p, m, l, r) =>
         let
            val l' = mapi f l
            val r' = mapi f r
         in
            bin (p, m, l, r)
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
                  (acc', tip (k, x'))
               end
          | Bin (p, m, l, r) =>
               let
                  val (acc', l') = go (l, acc)
                  val (acc'', r') = go (r, acc')
               in
                  (acc'', bin (p, m, l, r))
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
                  (acc', tip (k, x'))
               end
          | Bin (p, m, l, r) =>
               let
                  val (acc', l') = go (l, acc)
                  val (acc'', r') = go (r, acc')
               in
                  (acc'', bin (p, m, l, r))
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

fun toSeq t =
   let
      fun go (t, acc) () =
         case node t of
            Nil => acc ()
          | Tip (k, x) => Seq.Cons ((k, x), acc)
          | Bin (_, _, l, r) => go (l, go (r, acc)) ()
   in
      go (t, Seq.empty)
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
                  bin (p, m, l', r')
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
                            | SOME y => tip (k, y))
          | Bin (p, m, l, r) =>
               let
                  val l' = go l
                  val r' = go r
               in
                  bin (p, m, l', r')
               end
   in
      go
   end

fun mapPartial f = mapPartiali (fn (_, x) => f x)

fun mapEitheri f =
   let
      fun go t =
         case node t of
            Nil => (empty, empty)
          | Tip (k, x) =>
               (case f (k, x) of
                   Either.INL y => (tip (k, y), empty)
                 | Either.INR z => (empty, tip (k, z)))
          | Bin (p, m, l, r) =>
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
      fun go t =
         case node t of
            Nil => (empty, empty)
          | Tip (k, x) =>
               if p (k, x)
                  then (t, empty)
               else (empty, t)
          | Bin (p, m, l, r) =>
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

(* TODO: Is this the most efficient? *)
fun isSubmap (t1, t2) = equals (empty, difference (t1, t2))

fun isSubmapBy p =
   let
      fun go (t1, t2) =
         if equals (t1, t2)
            then true
         else
            case (node t1, node t2) of
               (Nil, _) => true
             | (_, Nil) => false
             | (Tip (k, x), _) =>
                  (case find (t2, k) of
                      NONE => false
                    | SOME y => p (x, y))
             | (Bin _, Tip _) => false
             | (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =>
                  if shorter (m1, m2)
                     then false
                  else if shorter (m2, m1)
                     then
                        if nomatch (p1, p2, m2)
                           then false
                        else if zero (p1, m2)
                           then go (t1, l2)
                        else go (t1, r2)
                  else
                     p1 = p2
                     andalso go (l1, l2)
                     andalso go (r1, r2)
   in
      go
   end

fun isProperSubmapBy p (t1, t2) =
   let
      datatype cmp = EQ | SUBSET | NOTSUBSET

      fun go (t1, t2) =
         case (node t1, node t2) of
            (Nil, Nil) => EQ
          | (Nil, _) => SUBSET
          | (Tip (kx, x), Tip (ky, y)) =>
               if kx = ky andalso p (x, y)
                  then EQ
               else NOTSUBSET
          | (Tip (k, x), _) =>
               (case find (t2, k) of
                   NONE => NOTSUBSET
                 | SOME y => if p (x, y) then SUBSET else NOTSUBSET)
          | (Bin _, Nil) => NOTSUBSET
          | (Bin _, Tip _) => NOTSUBSET
          | (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =>
               if shorter (m1, m2)
                  then NOTSUBSET
               else if shorter (m2, m1)
                  then
                     if nomatch (p1, p2, m2)
                        then NOTSUBSET
                     else if zero (p1, m2)
                        then go (t1, l2)
                     else go (t1, r2)
               else if p1 = m2
                  then 
                     case go (l1, l2) of
                        NOTSUBSET => NOTSUBSET
                      | EQ => go (r1, r2)
                      | SUBSET =>
                           case go (r1, r2) of
                              NOTSUBSET => NOTSUBSET
                            | _ => SUBSET
               else NOTSUBSET
   in
      go (t1, t2) = SUBSET
   end

fun liftEquals eq (t1, t2) =
   if equals (t1, t2)
      then true
   else
      case (node t1, node t2) of
         (Nil, Nil) => true
       | (Tip (kx, x), Tip (ky, y)) =>
            kx = ky andalso eq (x, y)
       | (Bin (p1, m1, l1, r1), Bin (p2, m2, l2, r2)) =>
            m1 = m2
            andalso p1 = p2
            andalso liftEquals eq (l1, l2)
            andalso liftEquals eq (r1, r2)
       | _ => false

fun collate cmp (t1, t2) =
   let
      fun go (Seq.Nil, Seq.Nil) = EQUAL
        | go (Seq.Nil, Seq.Cons _) = LESS
        | go (Seq.Cons _, Seq.Nil) = GREATER
        | go (Seq.Cons ((kx, x), xs), Seq.Cons ((ky, y), ys)) =
         case Word.compare (kx, ky) of
            EQUAL =>
               (case cmp (x, y) of
                  EQUAL => go (xs (), ys ())
                | order => order)
          | order => order
   in
      go (toSeq t1 (), toSeq t2 ())
   end

(** WordMap specific **)

fun viewMin t =
   if isEmpty t
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
   if isEmpty t
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

end

(* vim: set tw=0 ts=3 sw=3: *)
