
structure SparseVector :> SPARSE_VECTOR =
struct
   open Compat
   structure W = Word32
   structure V = Vector

   datatype 'a t = T of {
      mask: W.word,
      data: 'a V.vector
   }

   (* Count trailing zeros *)
   fun ctz 0w0 = 0w32
     | ctz w =
      let
        fun go (mask, set, w, n) =
           if W.andb (w, mask) = 0w0
              then (W.>> (w, set), n + set)
           else (w, n)

        val (w, n) = (w, 0w0)
        val (w, n) = go (0wx0000ffff, 0w16, w, n)
        val (w, n) = go (0wx000000ff, 0w8, w, n)
        val (w, n) = go (0wx0000000f, 0w4, w, n)
        val (w, n) = go (0wx00000003, 0w2, w, n)
        val (w, n) = go (0wx00000001, 0w1, w, n)
     in
        n
     end

   (* Count leading zeros *)
   fun clz 0w0 = 0w32
     | clz w =
      let
        fun go (mask, set, w, n) =
           if W.andb (w, mask) = 0w0
              then (W.<< (w, set), n + set)
           else (w, n)

        val n = 0w0
        val (w, n) = go (0wxffff0000, 0w16, w, n)
        val (w, n) = go (0wxff000000, 0w8, w, n)
        val (w, n) = go (0wxf0000000, 0w4, w, n)
        val (w, n) = go (0wxc0000000, 0w2, w, n)
        val (w, n) = go (0wx80000000, 0w1, w, n)
     in
        n
     end

   (* Convert from an index mask (1 << idx) to the backing vector index *)
   fun sparseIndex (mask, idxMask) =
      Compat.Word32.popCount (W.andb (mask, idxMask - 0w1))

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
                  let
                     val len' = V.length data + 1
                     val mask' = W.orb (mask, idxMask)
                     fun gen j =
                        case Int.compare (j, si) of
                           LESS => Vector.sub (data, j)
                         | EQUAL => x
                         | GREATER => Vector.sub (data, j - 1)
                  in
                     (T {mask = mask', data = V.tabulate (len', gen)}, NONE)
                  end
            else
               let
                  val old = V.sub (data, si)
                  val new = f (i, old, x)
               in
                  (T {mask = mask, data = V.update (data, si, new)}, SOME old)
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
                           fun gen j =
                              if j < si
                                 then V.sub (data, j)
                              else V.sub (data, j + 1)
                           val data' = V.tabulate (V.length data - 1, gen)
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
                  val len = V.length data
                  fun gen j =
                     if j < si
                        then Vector.sub (data, j)
                     else Vector.sub (data, j + 1)
                  val mask' = W.andb (mask, W.notb idxMask)
                  val data' = V.tabulate (len - 1, gen)
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
                           val len' = V.length data + 1
                           val mask' = W.orb (mask, idxMask)
                           fun gen j =
                              case Int.compare (j, si) of
                                 LESS => V.sub (data, j)
                               | EQUAL => new
                               | GREATER => V.sub (data, j - 1)
                        in
                           T {mask = mask', data = V.tabulate (len', gen)}
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
                           val len' = V.length data - 1
                           val mask' = W.andb (mask, W.notb idxMask)
                           fun gen j =
                              if j < si
                                 then V.sub (data, j)
                              else V.sub (data, j + 1)
                        in
                           T {mask = mask', data = V.tabulate (len', gen)}
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
               val i = ctz mask
               val mask' = W.andb (mask, W.notb (W.<< (0w1, i)))
            in
               f (Word.toInt i, V.sub (data, si))
               andalso go (mask', si + 1)
            end
      in
         go (mask, 0)
      end

   fun existsi f (T {data, mask}) =
      let
         fun go (0w0, _) = false
           | go (mask, si) =
            let
               val i = ctz mask
               val mask' = W.andb (mask, W.notb (W.<< (0w1, i)))
            in
               f (Word.toInt i, V.sub (data, si))
               orelse go (mask', si + 1)
            end
      in
         go (mask, 0)
      end

   fun foldli f z (T {data, mask}) =
      let
         fun go (0w0, _, acc) = acc
           | go (mask, si, acc) =
            let
               val i = ctz mask
               val mask' = W.andb (mask, W.notb (W.<< (0w1, i)))
            in
               go (mask', si + 1, f (Word.toInt i, V.sub (data, si), acc))
            end
      in
         go (mask, 0, z)
      end

   fun foldri f z (T {data, mask}) =
      let
         fun go (0w0, _, acc) = acc
           | go (mask, si, acc) =
            let
               val i = 0w31 - clz mask
               val mask' = W.andb (mask, W.notb (W.<< (0w1, i)))
            in
               go (mask', si - 1, f (Word.toInt i, V.sub (data, si), acc))
            end
      in
         go (mask, V.length data - 1, z)
      end

   fun mapi f (T {data, mask}) =
      let
         fun go (si, mask) =
            let
               val i = ctz mask
               val mask' = W.andb (mask, W.notb (W.<< (0w1, i)))
            in
               (f (Word.toInt i, V.sub (data, si)), mask')
            end
         val (d, mask') = Compat.Vector.unfoldi (V.length data, mask, go)
      in
         if mask' <> 0w0
            then raise Fail "SparseVector.mapi: wrong length"
         else ()
         ; T {mask = mask, data = d}
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
      case ctz mask of
         0w32 => NONE
       | i => SOME (Word.toInt i, V.sub (data, 0))

   fun findMax (T {data, mask}) =
      case clz mask of
         0w32 => NONE
       | i => SOME (31 - Word.toInt i, V.sub (data, V.length data - 1))

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
         val idx = ctz mask
         val mask' = W.andb (mask, W.notb (W.<< (0w1, idx)))
         fun gen i = V.sub (data, i + 1)
      in
         T {data = V.tabulate (V.length data - 1, gen), mask = mask'}
      end

   fun deleteMax (sv as T {mask = 0w0, ...}) = sv
     | deleteMax (T {data, mask}) =
      let
         val idx = 0w31 - clz mask
         val mask' = W.andb (mask, W.notb (W.<< (0w1, idx)))
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
               val idx' = ctz b
               val idx = Word.toInt idx'
               val m = W.<< (0w1, idx')
               val b' = W.andb (b, W.notb m)
            in
               if W.andb (m, W.andb (m1, m2)) <> 0w0
                  then (f (idx, V.sub (d1, i), V.sub (d2, j)), (i + 1, j + 1, b'))
               else if W.andb (m, m1) <> 0w0
                  then (V.sub (d1, i), (i + 1, j, b'))
               else (V.sub (d2, j), (i, j + 1, b'))
            end

         val both = W.orb (m1, m2)
         val len = Compat.Word32.popCount both

         val (v, (_, _, b)) = Compat.Vector.unfoldi (len, (0, 0, both), go o #2)
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
         val max1 = Word.toInt (0w32 - clz m1)
         val max2 = Word.toInt (0w32 - clz m2)
         val min = Word.toInt (ctz (W.orb (m1, m2)))

         fun go i =
            (print ("i = " ^ Int.toString i ^ "\n");
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
                         )
      in
         go min
      end
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
