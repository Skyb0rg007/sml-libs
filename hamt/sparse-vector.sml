
structure SparseVector :> SPARSE_VECTOR =
struct
   open Util
   structure W = Word32
   structure V = Vector

   datatype 'a t = T of {
      mask: W.word,
      data: 'a V.vector
   }

   (* Convert from an index mask (1 << idx) to the backing vector index *)
   fun sparseIndex (mask, idxMask) =
      W.popCount (W.andb (mask, idxMask - 0w1))

   val maxLen = 32

   val empty = T { mask = 0w0, data = #[] }

   fun length (T {data, ...}) = V.length data

   fun isEmpty (T {mask, ...}) = mask = 0w0

   fun singleton (i, v) =
      if i < 0 orelse i >= maxLen
         then raise Subscript
      else
         T {
            mask = W.<< (0w1, Word.fromInt i),
            data = #[v]
         }

   fun insertLookupWithi f (T {mask, data}, i, x) =
      if i < 0 orelse i >= maxLen
         then raise Subscript
      else
         let
            val idxMask = W.<< (0w1, Word.fromInt i)
            val si = sparseIndex (mask, idxMask)
         in
            if W.andb (mask, idxMask) = 0w0
               then
                  (T {mask = W.orb (mask, idxMask),
                      data = V.insert (data, si, x)},
                   NONE)
            else
               let
                  val old = V.sub (data, si)
                  val new = f (i, old, x)
               in
                  (T {mask = mask,
                      data = V.update (data, si, new)},
                   SOME old)
               end
         end

   fun insertWithi f args = #1 (insertLookupWithi f args)

   fun insertWith f args = insertWithi (fn (_, x', x) => f (x', x)) args

   fun insert args = insertWithi (fn (_, _, x) => x) args

   fun updateLookupi f (sv as T {mask, data}, i) =
      if i < 0 orelse i >= maxLen
         then raise Subscript
      else
         let
            val idxMask = W.<< (0w1, Word.fromInt i)
            val si = sparseIndex (mask, idxMask)
         in
            if W.andb (mask, idxMask) = 0w0
               then (sv, NONE)
            else
               let
                  val old = V.sub (data, si)
               in
                  case f (i, old) of
                     SOME new =>
                        let
                           val data' = V.update (data, si, new)
                        in
                           (T {mask = mask, data = data'}, SOME old)
                        end
                   | NONE =>
                        let
                           val mask' = W.andb (mask, W.notb idxMask)
                           val data' = V.remove (data, si)
                        in
                           (T {mask = mask', data = data'}, SOME old)
                        end
               end
         end

   fun updatei f (sv, i) = #1 (updateLookupi f (sv, i))

   fun update f (sv, i) = updatei (fn (_, x) => f x) (sv, i)

   fun adjusti f (sv as T {mask, data}, i) =
      if i < 0 orelse i >= maxLen
         then raise Subscript
      else
         let
            val idxMask = W.<< (0w1, Word.fromInt i)
         in
            if W.andb (mask, idxMask) = 0w0
               then sv
            else
               let
                  val si = sparseIndex (mask, idxMask)
                  val old = V.sub (data, si)
               in
                  T {mask = mask, data = V.update (data, si, f (i, old))}
               end
         end

   fun adjust f (sv, i) = adjusti (fn (_, x) => f x) (sv, i)

   fun remove (T {mask, data}, i) =
      if i < 0 orelse i >= maxLen
         then raise Subscript
      else
         let
            val idxMask = W.<< (0w1, Word.fromInt i)
         in
            if W.andb (mask, idxMask) = 0w0
               then NONE
            else
               let
                  val si = sparseIndex (mask, idxMask)
                  val mask' = W.andb (mask, W.notb idxMask)
                  val data' = V.remove (data, si)
               in
                  SOME (T {mask = mask', data = data'}, Vector.sub (data, si))
               end
         end

   fun delete (sv, i) =
      case remove (sv, i) of
         NONE => sv
       | SOME (sv', _) => sv'

   fun alteri f (sv as T {mask, data}, i) =
      if i < 0 orelse i >= maxLen
         then raise Subscript
      else
         let
            val idxMask = W.<< (0w1, Word.fromInt i)
         in
            if W.andb (mask, idxMask) = 0w0
               then
                  case f (i, NONE) of
                     NONE => sv
                   | SOME new =>
                        let
                           val si = sparseIndex (mask, idxMask)
                           val mask' = W.orb (mask, idxMask)
                           val data' = V.insert (data, si, new)
                        in
                           T {mask = mask', data = data'}
                        end
            else
               let
                  val si = sparseIndex (mask, idxMask)
                  val old = V.sub (data, si)
               in
                  case f (i, SOME old) of
                     SOME new =>
                        T {mask = mask, data = V.update (data, si, new)}
                   | NONE =>
                        let
                           val mask' = W.andb (mask, W.notb idxMask)
                        in
                           T {mask = mask', data = V.remove (data, si)}
                        end
               end
         end

   fun alter f (sv, i) = alteri (fn (_, x) => f x) (sv, i)

   fun find (T {mask, data}, i) =
      if i < 0 orelse i >= maxLen
         then raise Subscript
      else
         let
            val idxMask = W.<< (0w1, Word.fromInt i)
         in
            if W.andb (mask, idxMask) = 0w0
               then NONE
            else
               SOME (V.sub (data, sparseIndex (mask, idxMask)))
         end

   fun lookup (sv, i) =
      case find (sv, i) of
         NONE => raise Subscript
       | SOME x => x

   fun inDomain (T {mask, ...}, i) =
      if i < 0 orelse i >= maxLen
         then raise Subscript
      else W.andb (mask, W.<< (0w1, Word.fromInt i)) <> 0w0

   fun fromListWithi f xs =
      List.foldl (fn ((k, v), acc) => insertWithi f (acc, k, v)) empty xs

   fun fromListWith f xs =
      List.foldl (fn ((k, v), acc) => insertWith f (acc, k, v)) empty xs

   fun fromList xs =
      List.foldl (fn ((k, v), acc) => insert (acc, k, v)) empty xs

   fun disjoint (T {mask = m1, ...}, T {mask = m2, ...}) =
      W.andb (m1, m2) = 0w0

   fun map f (T {mask, data}) = T {mask = mask, data = V.map f data}

   fun foldl f z (T {data, ...}) = V.foldl f z data

   fun foldr f z (T {data, ...}) = V.foldr f z data

   fun all f (T {data, ...}) = V.all f data

   fun exists f (T {data, ...}) = V.exists f data

   fun listItems (T {data, ...}) = V.foldr op:: [] data

   fun alli f (T {data, mask}) =
      let
         fun go (0w0, _) = true
           | go (mask, si) =
            let
               val i = W.trailingZeros mask
               val mask' = W.andb (mask, W.notb (W.<< (0w1, Word.fromInt i)))
            in
               f (i, V.sub (data, si)) andalso go (mask', si + 1)
            end
      in
         go (mask, 0)
      end

   fun existsi f (T {data, mask}) =
      let
         fun go (0w0, _) = false
           | go (mask, si) =
            let
               val i = W.trailingZeros mask
               val mask' = W.andb (mask, W.notb (W.<< (0w1, Word.fromInt i)))
            in
               f (i, V.sub (data, si)) orelse go (mask', si + 1)
            end
      in
         go (mask, 0)
      end

   fun foldli f z (T {data, mask}) =
      let
         fun go (0w0, _, acc) = acc
           | go (mask, si, acc) =
            let
               val i = W.trailingZeros mask
               val mask' = W.andb (mask, W.notb (W.<< (0w1, Word.fromInt i)))
            in
               go (mask', si + 1, f (i, V.sub (data, si), acc))
            end
      in
         go (mask, 0, z)
      end

   fun foldri f z (T {data, mask}) =
      let
         fun go (0w0, _, acc) = acc
           | go (mask, si, acc) =
            let
               val i = 31 - W.leadingZeros mask
               val mask' = W.andb (mask, W.notb (W.<< (0w1, Word.fromInt i)))
            in
               go (mask', si - 1, f (i, V.sub (data, si), acc))
            end
      in
         go (mask, V.length data - 1, z)
      end

   fun mapi f (T {data, mask}) =
      let
         fun go (si, mask) =
            let
               val i = W.trailingZeros mask
               val mask' = W.andb (mask, W.notb (W.<< (0w1, Word.fromInt i)))
            in
               (f (i, V.sub (data, si)), mask')
            end
         val (d, mask') = V.unfoldi (V.length data, mask, go)
      in
         if mask' <> 0w0
            then raise Fail "SparseVector.mapi: wrong length"
         else ()
         ; T {mask = mask, data = d}
      end

   fun mapAccuml f acc (T {data, mask}) =
      let
         fun go (i, acc) = f (V.sub (data, i), acc)
         val (data', acc') = V.unfoldi (V.length data, acc, go)
      in
         (acc', T {data = data', mask = mask})
      end

   fun mapPartiali f (T {data, mask}) =
      let
         val n = V.length data
         val arr = Array.array (n, NONE)
         fun step (si, size, mask, idx, idxMask) =
            if si >= n
               then (mask, size)
            else 
               let
                  val idx' = idx + 1
                  val idxMask' = W.<< (idxMask, 0w1)
               in
                  if W.andb (mask, idxMask) = 0w0
                     then step (si, size, mask, idx + 1, idxMask')
                  else
                     case f (idx, V.sub (data, si)) of
                        NONE =>
                           step (si + 1, size, W.andb (mask, W.notb idxMask), idx + 1, idxMask')
                      | SOME x =>
                           (Array.update (arr, size, SOME x)
                            ; step (si + 1, size + 1, mask, idx + 1, idxMask'))
               end
         val (mask', size) = step (0, 0, mask, 0, 0w1)
         fun gen i = Option.valOf (Array.sub (arr, i))
      in
         T {mask = mask', data = V.tabulate (size, gen) }
      end

   fun mapPartial f = mapPartiali (fn (_, v) => f v)

   fun listItemsi sv = foldri (fn (k, v, acc) => (k, v) :: acc) [] sv

   fun listKeys sv = foldri (fn (k, _, acc) => k :: acc) [] sv

   fun findMin (T {data, mask}) =
      case W.trailingZeros mask of
         32 => NONE
       | i => SOME (i, V.sub (data, 0))

   fun findMax (T {data, mask}) =
      case W.leadingZeros mask of
         32 => NONE
       | i => SOME (31 - i, V.sub (data, V.length data - 1))

   fun lookupMin sv =
      case findMin sv of
         NONE => raise Fail "SparseVector.lookupMin: Empty vector"
       | SOME (i, x) => (i, x)

   fun lookupMax sv =
      case findMax sv of
         NONE => raise Fail "SparseVector.lookupMax: Empty vector"
       | SOME (i, x) => (i, x)

   fun deleteMin (sv as T {mask = 0w0, ...}) = sv
     | deleteMin (T {data, mask}) =
      let
         val idx = W.trailingZeros mask
         val mask' = W.andb (mask, W.notb (W.<< (0w1, Word.fromInt idx)))
         fun gen i = V.sub (data, i + 1)
      in
         T {data = V.tabulate (V.length data - 1, gen), mask = mask'}
      end

   fun deleteMax (sv as T {mask = 0w0, ...}) = sv
     | deleteMax (T {data, mask}) =
      let
         val idx = 31 - W.leadingZeros mask
         val mask' = W.andb (mask, W.notb (W.<< (0w1, Word.fromInt idx)))
         fun gen i = V.sub (data, i)
      in
         T {data = V.tabulate (V.length data - 1, gen), mask = mask'}
      end

   fun isSubmapBy cmp (T {mask = m1, data = d1}, T {mask = m2, data = d2}) =
      let
         val either = W.orb (m1, m2)
         val both = W.andb (m1, m2)

         fun go (i, j, m) =
            if m > either orelse m = 0w0
               then true
            else if W.andb (both, m) <> 0w0
               then
                  cmp (V.sub (d1, i), V.sub (d2, j))
                  andalso go (i + 1, j + 1, W.<< (m, 0w1))
            else if W.andb (m2, m) <> 0w0
               then go (i, j + 1, W.<< (m, 0w1))
            else go (i, j, W.<< (m, 0w1))
      in
         either = m2 andalso go (0, 0, W.andb (m2, W.notb m2 + 0w1))
      end

   fun isProperSubmapBy cmp (sv1, sv2) =
      length sv1 < length sv2 andalso isSubmapBy cmp (sv1, sv2)

   fun unionWithi f (T {mask = m1, data = d1}, T {mask = m2, data = d2}) =
      let
         fun go (_, _, 0w0) = raise Fail "SparseVector.unionWithi: length too short"
           | go (i, j, b) =
            let
               val idx = W.trailingZeros b
               val m = W.<< (0w1, Word.fromInt idx)
               val b' = W.andb (b, W.notb m)
            in
               if W.andb (m, W.andb (m1, m2)) <> 0w0
                  then (f (idx, V.sub (d1, i), V.sub (d2, j)), (i + 1, j + 1, b'))
               else if W.andb (m, m1) <> 0w0
                  then (V.sub (d1, i), (i + 1, j, b'))
               else (V.sub (d2, j), (i, j + 1, b'))
            end

         val both = W.orb (m1, m2)
         val len = W.popCount both

         val (v, (_, _, b)) = V.unfoldi (len, (0, 0, both), go o #2)
      in
         if b <> 0w0
            then raise Fail "SparseVector.unionWithi: length too long"
         else ()
         ; T {mask = both, data = v}
      end

   fun unionWith f svs = unionWithi (fn (_, x, y) => f (x, y)) svs

   fun union svs = unionWithi (fn (_, x, _) => x) svs

   fun same eq (T {mask = m1, data = d1}, T {mask = m2, data = d2}) =
      let
         val len = V.length d1
         fun go i =
            i >= len
            orelse eq (V.sub (d1, i), V.sub (d2, i)) andalso go (i + 1)
      in
         m1 = m2 andalso go 0
      end

   fun collate cmp (sv1 as T {mask = m1, ...}, sv2 as T {mask = m2, ...}) =
      let
         val max1 = 32 - W.leadingZeros m1
         val max2 = 32 - W.leadingZeros m2
         val min = W.trailingZeros (W.orb (m1, m2))

         fun go i =
            case (i >= max1, i >= max2) of
               (true, true) => EQUAL
             | (true, false) => LESS
             | (false, true) => GREATER
             | (false, false) =>
                  case (find (sv1, i), find (sv2, i)) of
                     (NONE, NONE) => go (i + 1)
                   | (NONE, SOME _) => LESS
                   | (SOME _, NONE) => GREATER
                   | (SOME x, SOME y) =>
                        case cmp (x, y) of
                           EQUAL => go (i + 1)
                         | order => order
      in
         go min
      end
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
