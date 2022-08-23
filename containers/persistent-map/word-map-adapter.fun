
signature WORD_KEY =
   sig
      type t

      val toWord: t -> word
   end

functor WordMapAdapterFn(Key: WORD_KEY) :> MAP where type key = Key.t =
struct
   
structure M = WordMap

type key = Key.t
type 'a map = (key * 'a) M.map

val empty = M.empty

fun singleton (k, x) = M.singleton (Key.toWord k, (k, x))

fun insertLookupWithi f (t, k, x) =
   let
      fun f' (_, (k, x), (_, y)) = (k, f (k, x, y))
      val (old, new) = M.insertLookupWithi f' (t, Key.toWord k, (k, x))
   in
      (Option.map #2 old, new)
   end

fun insertWithi f (t, k, x) =
   M.insertWith (fn ((k, x), (_, y)) => (k, f (k, x, y))) (t, Key.toWord k, (k, x))

fun insertWith f (t, k, x) =
   M.insertWith (fn ((k, x), (_, y)) => (k, f (x, y))) (t, Key.toWord k, (k, x))

fun insert (t, k, x) = M.insert (t, Key.toWord k, (k, x))

fun insert' ((k, x), t) = insert (t, k, x)

fun fromList xs = List.foldl insert' empty xs

fun fromListWithi f xs =
   List.foldl (fn ((k, x), t) => insertWithi f (t, k, x)) empty xs

fun fromListWith f xs =
   List.foldl (fn ((k, x), t) => insertWith f (t, k, x)) empty xs

fun delete (t, k) = M.delete (t, Key.toWord k)

fun remove (t, k) =
   case M.remove (t, Key.toWord k) of
      NONE => NONE
    | SOME (m, (_, x)) => SOME (m, x)

fun adjust f (t, k) = M.adjust (fn (k, x) => (k, f x)) (t, Key.toWord k)

fun update f (t, k) =
   let
      fun f' (k, x) =
         case f x of
            NONE => NONE
          | SOME y => SOME (k, y)
   in
      M.update f' (t, Key.toWord k)
   end

fun updateLookupWithi f (t, k) =
   let
      fun f' (_, (k, x)) =
         case f (k, x) of
            NONE => NONE
          | SOME y => SOME (k, y)

      val (old, map) = M.updateLookupWithi f' (t, Key.toWord k)
   in
      (Option.map #2 old, map)
   end

fun alter f (t, k) =
   let
      fun f' NONE =
         (case f NONE of
             NONE => NONE
           | SOME y => SOME (k, y))
        | f' (SOME (k, x)) =
         case f (SOME x) of
            NONE => NONE
          | SOME y => SOME (k, y)
   in
      M.alter f' (t, Key.toWord k)
   end

fun find (t: 'a map, k) = Option.map #2 (M.find (t, Key.toWord k))

fun inDomain (t, k) = M.inDomain (t, Key.toWord k)

val isEmpty = M.isEmpty

val size = M.size

fun unionWithi f = M.unionWith (fn ((k, x), (_, y)) => (k, f (k, x, y)))
fun unionWith f = M.unionWith (fn ((k, x), (_, y)) => (k, f (x, y)))
val difference = M.difference
fun differenceWithi f = M.differenceWith (fn ((k, x), (_, y)) => Option.map (fn z => (k, z)) (f (k, x, y)))
fun differenceWith f = M.differenceWith (fn ((k, x), (_, y)) => Option.map (fn z => (k, z)) (f (x, y)))
val intersection = M.intersection
fun intersectionWithi f = M.intersectionWith (fn ((k, x), (_, y)) => Option.map (fn z => (k, z)) (f (k, x, y)))
fun intersectionWith f = M.intersectionWith (fn ((k, x), (_, y)) => Option.map (fn z => (k, z)) (f (x, y)))
val disjoint = M.disjoint
fun map f = M.map (fn (k, x) => (k, f x))
fun mapi f = M.map (fn (k, x) => (k, f (k, x)))
fun mapAccumL f = M.mapAccumL (fn ((k, x), z) => let val (z, y) = f (x, z) in (z, (k, y)) end)
fun mapAccumLi f = M.mapAccumL (fn ((k, x), z) => let val (z, y) = f (k, x, z) in (z, (k, y)) end)
fun mapAccumR f = M.mapAccumR (fn ((k, x), z) => let val (z, y) = f (x, z) in (z, (k, y)) end)
fun mapAccumRi f = M.mapAccumR (fn ((k, x), z) => let val (z, y) = f (k, x, z) in (z, (k, y)) end)
fun foldl f = M.foldl (fn ((_, x), z) => f (x, z))
fun foldli f = M.foldl (fn ((k, x), z) => f (k, x, z))
fun foldr f = M.foldr (fn ((_, x), z) => f (x, z))
fun foldri f = M.foldr (fn ((k, x), z) => f (k, x, z))
fun app f = M.app (fn (_, x) => f x)
val appi = M.app
fun exists f = M.exists (fn (_, x) => f x)
val existsi = M.exists
fun all f = M.all (fn (_, x) => f x)
val alli = M.all
fun keys m = M.foldr (fn ((k, _), l) => k :: l) [] m
fun elems m = M.foldr (fn ((_, x), l) => x :: l) [] m
val toList = M.elems
fun filter f = M.filter (fn (_, x) => f x)
val filteri = M.filter
fun mapPartial f = M.mapPartial (fn (k, x) => Option.map (fn z => (k, z)) (f x))
fun mapPartiali f = M.mapPartial (fn (k, x) => Option.map (fn z => (k, z)) (f (k, x)))
fun mapEither f = M.mapEither (fn (k, x) => Either.map (fn z => (k, z), fn z => (k, z)) (f x))
fun mapEitheri f = M.mapEither (fn (k, x) => Either.map (fn z => (k, z), fn z => (k, z)) (f (k, x)))
fun partition f = M.partition (fn (_, x) => f x)
val partitioni = M.partition
fun isSubmapBy f = M.isSubmapBy (fn ((_, x), (_, y)) => f (x, y))
fun isProperSubmapBy f = M.isProperSubmapBy (fn ((_, x), (_, y)) => f (x, y))
fun liftEquals eq = M.liftEquals (fn ((_, x), (_, y)) => eq (x, y))
fun collate cmp = M.collate (fn ((_, x), (_, y)) => cmp (x, y))

end

(* vim: set tw=0 sw=3 ts=3: *)
