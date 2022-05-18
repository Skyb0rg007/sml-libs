
functor VectorMapFn(Key: EQ_KEY) : EQ_MAP =
struct
   open Compat
   structure V = Vector
   structure Key = Key

   datatype 'a map = T of (Key.eq_key * 'a) V.vector

   val empty = T #[]

   fun singleton (k, v) = T #[(k, v)]

   fun insertLookupWithi f (T vec, k, v) =
      let
         val len = V.length vec
         fun go i =
            if i >= len
               then (T (V.append (vec, (k, v))), NONE)
            else
               let
                  val (k', v') = V.sub (vec, i)
               in
                  if Key.sameKey (k, k')
                     then (T (V.update (vec, i, (k, f (k, v', v)))), SOME v')
                  else go (i + 1)
               end
      in
         go 0
      end

   fun insertWithi f (m, k, v) = #1 (insertLookupWithi f (m, k, v))

   fun insertWith f = insertWithi (fn (_, v', v) => f (v', v))

   fun insert (m, k, v) = insertWithi (fn (_, _, v) => v) (m, k, v)

   fun insert' ((k, v), m) = insert (m, k, v)

   fun fromListWithi f =
      List.foldl (fn ((k, v), m) => insertWithi f (m, k, v)) empty

   fun fromListWith f =
      List.foldl (fn ((k, v), m) => insertWith f (m, k, v)) empty

   fun fromList xs = List.foldl insert' empty xs

   fun fromDistinctList xs = T (V.fromList xs)

   fun find (T vec, k) =
      case V.find (fn (k', _) => Key.sameKey (k, k')) vec of
         NONE => NONE
       | SOME (_, v) => SOME v

   fun lookup (m, k) =
      case find (m, k) of
         NONE => raise Subscript
       | SOME v => v

   fun inDomain (m, k) = Option.isSome (find (m, k))

   fun size (T vec) = V.length vec

   fun isEmpty (T vec) = V.length vec = 0

   fun unionWithi _ (T #[], m2) = m2
     | unionWithi _ (m1, T #[]) = m1
     | unionWithi f (T vec1, T vec2) =
      let
         val n1 = V.length vec1
         val n2 = V.length vec2
         fun indexOf1 k =
            Option.map #1 (V.findi (fn (_, (k', _)) => Key.sameKey (k, k')) vec1)
         val indices = V.map (fn (k, _) => indexOf1 k) vec2
         val nOnly2 = V.foldl (fn (NONE, n) => n + 1 | (SOME _, n) => n) 0 indices
         val arr = Array.array (n1 + nOnly2, V.sub (vec1, 0))
         val () = Array.copyVec {dst = arr, src = vec1, di = 0}

         fun go (iEnd, i2) =
            if i2 >= n2
               then ()
            else
               case V.sub (indices, i2) of
                  NONE =>
                     (Array.update (arr, iEnd, V.sub (vec2, i2))
                      ; go (iEnd + 1, i2 + 1))
                | SOME i1 =>
                     let
                        val (k, v1) = V.sub (vec1, i1)
                        val (_, v2) = V.sub (vec2, i2)
                     in
                        Array.update (arr, i1, (k, f (k, v1, v2)))
                        ; go (iEnd, i2 + 1)
                     end
      in
         go (n1, 0)
         ; T (Array.vector arr)
      end

   fun unionWith f = unionWithi (fn (_, v1, v2) => f (v1, v2))

   fun union (m1, m2) = unionWithi (fn (_, v1, _) => v1) (m1, m2)

   fun remove (T vec, k) =
      case V.findi (fn (_, (k', _)) => Key.sameKey (k, k')) vec of
         NONE => NONE
       | SOME (i, (_, v')) =>
            let
               fun gen j =
                  if j < i
                     then V.sub (vec, j)
                  else V.sub (vec, j + 1)
            in
               SOME (T (V.tabulate (V.length vec - 1, gen)), v')
            end

   fun delete (m, k) =
      case remove (m, k) of
         NONE => m
       | SOME (m', _) => m'

   fun map f (T vec) =
      T (V.map (fn (k, v) => (k, f v)) vec)

   fun mapi f (T vec) =
      T (V.map (fn (k, v) => (k, f (k, v))) vec)

   fun mapPartiali f (T vec) =
      let
         val len = V.length vec
         val arr = Array.array (len, NONE)
         fun go (i, j) =
            if i >= len
               then j
            else
               let
                  val (k, v) = V.sub (vec, i)
               in
                  case f (k, v) of
                     NONE => go (i + 1, j)
                   | SOME v' => (Array.update (arr, j, SOME (k, v')); go (i + 1, j + 1))
               end
         val len' = go (0, 0)
      in
         T (V.tabulate (len', fn i => Option.valOf (Array.sub (arr, i))))
      end

   fun mapPartial f = mapPartiali (f o #2)

   fun foldl f z (T vec) =
      V.foldl (fn ((_, v), acc) => f (v, acc)) z vec

   fun foldli f z (T vec) =
      V.foldl (fn ((k, v), acc) => f (k, v, acc)) z vec

   fun foldr f z (T vec) =
      V.foldr (fn ((_, v), acc) => f (v, acc)) z vec

   fun foldri f z (T vec) =
      V.foldr (fn ((k, v), acc) => f (k, v, acc)) z vec

   fun all p (T vec) = V.all (p o #2) vec

   fun alli p (T vec) = V.all p vec

   fun exists p (T vec) = V.exists (p o #2) vec

   fun existsi p (T vec) = V.exists p vec

   fun listItems (T vec) = V.foldr (fn ((_, v), acc) => v :: acc) [] vec

   fun listItemsi (T vec) = V.foldr op:: [] vec

   fun listKeys (T vec) = V.foldr (fn ((k, _), acc) => k :: acc) [] vec

   fun isSubmapBy cmp (T vec1, m2 as T vec2) =
      let
         fun inVec2 (k, v) =
            case find (m2, k) of
               NONE => false
             | SOME v' => cmp (v, v')
      in
         V.length vec1 <= V.length vec2 andalso V.all inVec2 vec1
      end

   fun isProperSubmapBy cmp (T vec1, m2 as T vec2) =
      let
         fun inVec2 (k, v) =
            case find (m2, k) of
               NONE => false
             | SOME v' => cmp (v, v')
      in
         V.length vec1 = V.length vec2 andalso V.all inVec2 vec1
      end

   fun same sameVal (T vec1, T vec2) =
      let
         fun deleteBy eq (_, []) = NONE
           | deleteBy eq (x, y::ys) =
            if eq (x, y)
               then SOME ys
            else Option.map (fn ys' => y :: ys') (deleteBy eq (x, ys))

         fun sameKV ((k1, v1), (k2, v2)) =
            Key.sameKey (k1, k2) andalso sameVal (v1, v2)

         fun go ([], []) = true
           | go (_::_, []) = false
           | go ([], _::_) = false
           | go (x::xs, y::ys) =
            if sameKV (x, y)
               then go (xs, ys)
            else
               case deleteBy (Fn.flip sameKV) (y, xs) of
                  NONE => false
                | SOME xs' =>
                     case deleteBy sameKV (x, ys) of
                        NONE => false
                      | SOME ys' => go (xs', ys')
      in
         go (V.toList vec1, V.toList vec2)
      end

   fun collate cmpKey cmpVal (T vec1, T vec2) =
      let
         fun cmp ((k1, v1), (k2, v2)) =
            case cmpKey (k1, k2) of
               EQUAL => cmpVal (v1, v2)
             | order => order

         fun count p xs =
            V.foldr (fn (x, n) => if p x then n + 1 else n) 0 xs

         fun gtIn1 y = count (fn x => cmp (x, y) = LESS) vec1
         fun gtIn2 x = count (fn y => cmp (x, y) = GREATER) vec2

         fun ltIn1 y = count (fn x => cmp (x, y) = GREATER) vec1
         fun ltIn2 x = count (fn y => cmp (x, y) = LESS) vec2

         fun gt1 (x1, x2) = gtIn2 x1 > gtIn2 x2 andalso ltIn2 x1 < ltIn2 x2
         fun gt2 (y1, y2) = gtIn1 y1 > gtIn1 y2 andalso ltIn1 y1 < ltIn1 y2

         val list1 = ListMergeSort.sort gt1 (V.toList vec1)
         val list2 = ListMergeSort.sort gt2 (V.toList vec2)

         fun go ([], []) = EQUAL
           | go ([], _::_) = LESS
           | go (_::_, []) = GREATER
           | go (x::xs, y::ys) =
            case cmp (x, y) of
               EQUAL => go (xs, ys)
             | order => order
      in
         go (list1, list2)
      end
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
