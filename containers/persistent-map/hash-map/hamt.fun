
functor HAMTFn(Key: HASHABLE_TYPE): MAP =
struct

type key = Key.t

datatype 'a map =
   Empty
 | Bitmap of word * 'a map vector
 | Full of 'a map vector
 | Leaf of word * key * 'a
 | Collision of word * (key * 'a) vector

(* Helpers *)

fun vectorInsert (v, i, x) =
   let
      fun go idx =
         case Int.compare (idx, i) of
            LESS => Vector.sub (v, idx)
          | EQUAL => x
          | GREATER => Vector.sub (v, idx - 1)
   in
      Vector.tabulate (Vector.length v + 1, go)
   end

fun vectorDelete (v, i) =
   let
      fun go idx =
         if idx < i
            then Vector.sub (v, idx)
         else Vector.sub (v, idx + 1)
   in
      Vector.tabulate (Vector.length v - 1, go)
   end

fun vectorSnoc (v, x) =
   let
      val len = Vector.length v

      fun go idx =
         if idx = len
            then x
         else Vector.sub (v, idx)
   in
      Vector.tabulate (Vector.length v + 1, go)
   end

fun vectorEquals eq (v1, v2) =
   let
      val len1 = Vector.length v1
      val len2 = Vector.length v2

      fun go i =
         if i = len1
            then true
         else eq (Vector.sub (v1, i), Vector.sub (v2, i)) andalso go (i + 1)
   in
      len1 = len2 andalso go 0
   end

val bitsPerSubkey = 0w5
val subkeyMask = 0wx1f
val maxChildren = 0w32
val fullNodeMask = 0wxffffffff

(* Bitwise *)
fun index (w, s) = Word.andb (Word.>> (w, s), subkeyMask)
fun index' (w, s) = Word.toInt (index (w, s))
fun mask (w, s) = Word.<< (0w1, index (w, s))
fun sparseIndex (b, m) = Word.popCount (Word.andb (b, m - 0w1))

(* Create a hashmap from two hash-key-value triples at the given shift level *)
fun two (s, (h, k, x), (h', k', x')) =
   let
      val leaf = Leaf (h, k, x)
      val leaf' = Leaf (h', k', x')

      fun go s =
         let
            val b = mask (h, s)
            val b' = mask (h', s)
         in
            if b = b'
               then Bitmap (b, #[go (s + bitsPerSubkey)])
            else if index (h, s) < index (h', s)
               then Bitmap (Word.orb (b, b'), #[leaf, leaf'])
            else Bitmap (Word.orb (b, b'), #[leaf', leaf])
         end
   in
      go s
   end

fun bitmap (b, arr) =
   if b = fullNodeMask
      then Full arr
   else Bitmap (b, arr)

fun isLeafOrCollision (Leaf _) = true
  | isLeafOrCollision (Collision _) = true
  | isLeafOrCollision _ = false

(** Exports **)

val empty = Empty

fun singleton (k, x) = Leaf (Key.hash k, k, x)

fun insertLookupCollisionWithi f (kxs, k, x) =
   let
      val len = Vector.length kxs
      fun go i =
         if i >= len
            then (NONE, vectorSnoc (kxs, (k, x)))
         else if Key.equals (k, #1 (Vector.sub (kxs, i)))
            then (SOME (#2 (Vector.sub (kxs, i))), Vector.update (kxs, i, (k, x)))
         else go (i + 1)
   in
      go 0
   end

fun insertCollisionWithi f (kxs, k, x) =
   let
      val len = Vector.length kxs
      fun go i =
         if i >= len
            then vectorSnoc (kxs, (k, x))
         else if Key.equals (k, #1 (Vector.sub (kxs, i)))
            then Vector.update (kxs, i, (k, x))
         else go (i + 1)
   in
      go 0
   end

fun insertLookupWithi f (t, k, x) =
   let
      val h = Key.hash k

      fun go (_, Empty) = (NONE, Leaf (h, k, x))
        | go (s, Leaf (h', k', x')) =
         if h = h'
            then
               if Key.equals (k, k')
                  then (SOME x', Leaf (h, k, f (k, x', x)))
               else (NONE, Collision (h, #[(k', x'), (k, x)]))
         else (NONE, two (s, (h, k, x), (h', k', x')))
        | go (s, coll as Collision (h', kxs)) =
         if h = h'
            then 
               let
                  val (prev, kxs') = insertLookupCollisionWithi f (kxs, k, x)
               in
                  (prev, Collision (h, kxs'))
               end
         else go (s, Bitmap (mask (h', s), #[coll]))
        | go (s, Bitmap (b, arr)) =
         let
            val m = mask (h, s)
            val i = sparseIndex (b, m)
         in
            if Word.andb (b, m) <> 0w0
               then (NONE, bitmap (Word.orb (b, m), vectorInsert (arr, i, Leaf (h, k, x))))
            else
               let
                  val st = Vector.sub (arr, i)
                  val (prev, st') = go (s + bitsPerSubkey, st)
               in
                  (prev, Bitmap (b, Vector.update (arr, i, st')))
               end
         end
        | go (s, Full arr) =
         let
            val i = index' (h, s)
            val st = Vector.sub (arr, i)
            val (prev, st') = go (s + bitsPerSubkey, st)
         in
            (prev, Full (Vector.update (arr, i, st')))
         end
   in
      go (0w0, t)
   end

fun insertWithi f (t, k, x) =
   let
      val h = Key.hash k

      fun go (_, Empty) = Leaf (h, k, x)
        | go (s, Leaf (h', k', x')) =
         if h = h'
            then
               if Key.equals (k, k')
                  then Leaf (h, k, f (k, x', x))
               else Collision (h, #[(k', x'), (k, x)])
         else two (s, (h, k, x), (h', k', x'))
        | go (s, coll as Collision (h', kxs)) =
         if h = h'
            then Collision (h, insertCollisionWithi f (kxs, k, x))
         else go (s, Bitmap (mask (h', s), #[coll]))
        | go (s, Bitmap (b, arr)) =
         let
            val m = mask (h, s)
            val i = sparseIndex (b, m)
         in
            if Word.andb (b, m) <> 0w0
               then bitmap (Word.orb (b, m), vectorInsert (arr, i, Leaf (h, k, x)))
            else
               let
                  val st = Vector.sub (arr, i)
                  val st' = go (s + bitsPerSubkey, st)
               in
                  Bitmap (b, Vector.update (arr, i, st'))
               end
         end
        | go (s, Full arr) =
         let
            val i = index' (h, s)
            val st = Vector.sub (arr, i)
            val st' = go (s + bitsPerSubkey, st)
         in
            Full (Vector.update (arr, i, st'))
         end
   in
      go (0w0, t)
   end

fun insertWith f = insertWithi (fn (_, x', x) => f (x', x))
fun insert (t, k, x) = insertWithi (fn (_, _, x) => x) (t, k, x)
fun insert' ((k, x), t) = insert (t, k, x)

fun fromList xs = List.foldl insert' empty xs
fun fromListWith f xs = List.foldl (fn ((k, x), m) => insertWith f (m, k, x)) empty xs
fun fromListWithi f xs = List.foldl (fn ((k, x), m) => insertWithi f (m, k, x)) empty xs

fun remove (t, k) =
   let
      val h = Key.hash k

      fun go (_, Empty) = NONE
        | go (_, Leaf (h', k', x)) =
         if h = h' andalso Key.equals (k, k')
            then SOME (Empty, x)
         else NONE
        | go (s, Collision (h', kxs)) =
         if h = h'
            then
               case Vector.findi (fn (_, (k', _)) => Key.equals (k, k')) kxs of
                  NONE => NONE
                | SOME (i, (_, x)) =>
                     if Vector.length kxs <> 2
                        then SOME (Collision (h, vectorDelete (kxs, i)), x)
                     else 
                        let
                           val j = if i = 0 then 1 else 0
                           val (k', x') = Vector.sub (kxs, j)
                        in
                           SOME (Leaf (h, k', x'), x)
                        end
         else NONE
        | go (s, Full arr) =
         let
            val idx = index (h, s)
            val i = Word.toInt idx
            val st = Vector.sub (arr, i)
         in
            case go (s + bitsPerSubkey, st) of
               NONE => NONE
             | SOME (Empty, x) =>
                  let
                     val b = Word.andb (fullNodeMask, Word.notb (Word.<< (0w1, idx)))
                  in
                     SOME (Bitmap (b, vectorDelete (arr, i)), x)
                  end
             | SOME (st', x) => SOME (Full (Vector.update (arr, i, st')), x)
         end
        | go (s, Bitmap (b, arr)) =
         let
            val m = mask (h, s)
         in
            if Word.andb (b, m) = 0w0
               then NONE
            else
               let
                  val i = sparseIndex (b, m)
                  val st = Vector.sub (arr, i)
                  fun delIndex () =
                     Bitmap (Word.andb (b, Word.notb m), vectorDelete (arr, i))
               in
                  case (go (s + bitsPerSubkey, st), Vector.length arr) of
                     (NONE, _) => NONE
                     (* Preserve invariant: Nodes cannot have just 1 child if its not a node *)
                   | (SOME (Empty,              x), 1) => SOME (Empty, x)
                   | (SOME (st' as Leaf _,      x), 1) => SOME (st', x)
                   | (SOME (st' as Collision _, x), 1) => SOME (st', x)
                     (* The node had 2 children and it's being removed: check invariants *)
                   | (SOME (Empty, x), 2) =>
                        if i = 0 andalso isLeafOrCollision (Vector.sub (arr, 1))
                           then SOME (Vector.sub (arr, 1), x)
                        else if i = 1 andalso isLeafOrCollision (Vector.sub (arr, 0))
                           then SOME (Vector.sub (arr, 0), x)
                        else SOME (delIndex (), x)
                     (* Otherwise, just remove/update the index *)
                   | (SOME (Empty, x), _) => SOME (delIndex (), x)
                   | (SOME (st', x), _) => SOME (Bitmap (b, Vector.update (arr, i, st')), x)
               end
         end
   in
      go (0w0, t)
   end

fun delete (t, k) =
   case remove (t, k) of
      NONE => t
    | SOME (t', _) => t'

fun adjust f (t, k) =
   let
      val h = Key.hash k

      fun go (_, Empty) = Empty
        | go (_, t as Leaf (h', k', x)) =
         if h = h' andalso Key.equals (k, k')
            then Leaf (h, k, f x)
         else t
        | go (_, t as Collision (h', kxs)) =
         if h = h'
            then
               case Vector.findi (fn (_, (k', _)) => Key.equals (k, k')) kxs of
                  NONE => t
                | SOME (i, (_, x)) =>
                     Collision (h, Vector.update (kxs, i, (k, f x)))
         else t
        | go (s, t as Bitmap (b, arr)) =
         let
            val m = mask (h, s)
            val i = sparseIndex (b, m)
         in
            if Word.andb (b, m) = 0w0
               then t
            else
               let
                  val st = Vector.sub (arr, i)
                  val st' = go (s + bitsPerSubkey, st)
               in
                  Bitmap (b, Vector.update (arr, i, st'))
               end
         end
        | go (s, Full arr) =
         let
            val i = index' (h, s)
            val st = Vector.sub (arr, i)
            val st' = go (s + bitsPerSubkey, st)
         in
            Full (Vector.update (arr, i, st'))
         end
   in
      go (0w0, t)
   end

fun update _ = raise Fail "NYI"
fun updateLookupWithi _ = raise Fail "NYI"
fun alter _ = raise Fail "NYI"

fun find' (s, t, h, k) =
   let
      fun go (_, Empty) = NONE
        | go (_, Leaf (h', k', x)) =
         if h = h' andalso Key.equals (k, k')
            then SOME x
         else NONE
        | go (s, Bitmap (b, arr)) =
         let
            val m = mask (h, s)
         in
            if Word.andb (b, m) = 0w0
               then NONE
            else go (s + bitsPerSubkey, Vector.sub (arr, sparseIndex (b, m)))
         end
        | go (s, Full arr) =
         go (s + bitsPerSubkey, Vector.sub (arr, index' (h, s)))
        | go (s, Collision (h', kxs)) =
         if h = h'
            then
               case Vector.find (fn (k', _) => Key.equals (k, k')) kxs of
                  NONE => NONE
                | SOME (_, x) => SOME x
         else NONE
   in
      go (s, t)
   end

fun find (t, k) = find' (0w0, t, Key.hash k, k)

fun inDomain (t, k) = Option.isSome (find (t, k))

fun isEmpty Empty = true
  | isEmpty _ = false

fun size t =
   let
      fun go (Empty, n) = n
        | go (Leaf _, n) = n + 1
        | go (Bitmap (_, arr), n) = Vector.foldl go n arr
        | go (Full arr, n) = Vector.foldl go n arr
        | go (Collision (_, arr), n) = n + Vector.length arr
   in
      go (t, 0)
   end

fun unionWithi _ = raise Fail "NYI"
fun unionWith _ = raise Fail "NYI"
fun differenceWithi _ = raise Fail "NYI"
fun differenceWith _ = raise Fail "NYI"
fun difference _ = raise Fail "NYI"
fun intersectionWithi _ = raise Fail "NYI"
fun intersectionWith _ = raise Fail "NYI"
fun intersection _ = raise Fail "NYI"
fun disjoint _ = raise Fail "NYI"
fun mapi _ = raise Fail "NYI"
fun map _ = raise Fail "NYI"
fun mapAccumLi _ = raise Fail "NYI"
fun mapAccumL _ = raise Fail "NYI"
fun mapAccumRi _ = raise Fail "NYI"
fun mapAccumR _ = raise Fail "NYI"
fun foldli _ = raise Fail "NYI"
fun foldl _ = raise Fail "NYI"
fun foldri _ = raise Fail "NYI"
fun foldr _ = raise Fail "NYI"
fun appi _ = raise Fail "NYI"
fun app _ = raise Fail "NYI"
fun existsi _ = raise Fail "NYI"
fun exists _ = raise Fail "NYI"
fun alli _ = raise Fail "NYI"
fun all _ = raise Fail "NYI"
fun keys _ = raise Fail "NYI"
fun elems _ = raise Fail "NYI"
fun toList _ = raise Fail "NYI"
fun filteri _ = raise Fail "NYI"
fun filter _ = raise Fail "NYI"
fun mapPartiali _ = raise Fail "NYI"
fun mapPartial _ = raise Fail "NYI"
fun mapEitheri _ = raise Fail "NYI"
fun mapEither _ = raise Fail "NYI"
fun partitioni _ = raise Fail "NYI"
fun partition _ = raise Fail "NYI"
fun isSubmapBy _ = raise Fail "NYI"
fun isProperSubmapBy _ = raise Fail "NYI"
fun liftEquals _ = raise Fail "NYI"
fun collate _ = raise Fail "NYI"

end

(* vim: set tw=0 ts=3 sw=3: *)
