
signature BITSET =
   sig
      type t

      val bound: Word8.word

      val empty: t
      val isEmpty: t -> bool
      val universe: t
      val singleton: Word8.word -> t
      val insert: t * Word8.word -> t
      val remove: t * Word8.word -> t
      val member: t * Word8.word -> bool
      val union: t * t -> t
      val intersect: t * t -> t
      val difference: t * t -> t
      val disjoint: t * t -> bool
      val foldl: (Word8.word * 'a -> 'a) -> 'a -> t -> 'a
      val complement: t -> t
      val first: t -> Word8.word

      val compare: t * t -> order
      val same: t * t -> bool
   end

structure AtomicBitSet: BITSET =
   struct
      type t = Word.word

      val word8ToWord = Word.fromLargeWord o Word8.toLargeWord

      val bound = Word8.fromInt (Word.wordSize - 1)
      val empty = 0w0
      fun isEmpty w = w = 0w0
      fun singleton w = Word.<< (0w1, word8ToWord w)
      fun insert (s, w) = Word.orb (s, singleton w)
      fun member (s, w) = insert (s, w) = s
      fun remove (s, w) = Word.andb (s, Word.notb (singleton w))
      val union = Word.orb
      val intersect = Word.andb
      val complement = Word.notb
      val universe = Word.notb 0w0
      val disjoint = isEmpty o intersect
      val compare = Word.compare
      fun difference (s1, s2) = intersect (s1, complement s2)
      val same: t * t -> bool = op =

      fun tib w =
         let
            fun go (0w0, acc: Word8.word) = acc
              | go (w, acc) = go (Word.>> (w, 0w1), acc + 0w1)
         in
            go (w - 0w1, 0w0)
         end

      fun foldl _ z 0w0 = z
        | foldl f z s =
         let
            val x = Word.andb (s, Word.notb s + 0w1)
            val s = Word.xorb (s, x)
            val z = f (tib x, z)
         in
            foldl f z s
         end

      fun first 0w0 = raise Empty
        | first s = tib (Word.andb (s, Word.notb s + 0w1))
   end

structure Word8Set =
   struct
      structure V = Word8Vector

      type t = V.vector

      val BITS_PER_WORD = 8
      val WORDS = 32

      val empty = V.tabulate (32, fn _ => 0wx00)

      val universe = V.tabulate (32, fn _ => 0wxff)

      fun member (s, w) =
         let
            val index = Word8.toInt (Word8.div (w, 0w8))
            val offset = Word8.mod (w, 0w8)
            val offset = Word.fromLargeWord (Word8.toLargeWord offset)
            val mask = Word8.<< (0w1, offset)
         in
            Word8.andb (V.sub (s, index), mask) <> 0w0
         end

      fun add (s, w) =
         let
            val index = Word8.toInt (Word8.div (w, 0w8))
            val offset = Word8.mod (w, 0w8)
            val offset = Word.fromLargeWord (Word8.toLargeWord offset)
            val mask = Word8.<< (0w1, offset)
         in
            V.tabulate (32, fn i =>
               if i = index
                  then Word8.orb (V.sub (s, i), mask)
               else V.sub (s, i))
         end

      fun union (s1, s2) =
         V.tabulate (32, fn i =>
            Word8.orb (V.sub (s1, i), V.sub (s2, i)))

      fun intersect (s1, s2) =
         V.tabulate (32, fn i =>
            Word8.andb (V.sub (s1, i), V.sub (s2, i)))

      val compare = V.collate Word8.compare

      fun same (s1, s2) = compare (s1, s2) = EQUAL

      val isEmpty = V.all (fn w => w = 0w0)

      fun tib w =
         let
            fun go (0w0, acc: Word8.word) = acc
              | go (w, acc) = go (Word8.>> (w, 0w1), acc + 0w1)
         in
            go (w - 0w1, 0w0)
         end

      fun first s =
         let
            fun go i =
               if i >= 32
                  then NONE
               else if V.sub (s, i) = 0w0
                  then go (i + 1)
               else SOME (tib (V.sub (s, i)))
         in
            go 0
         end
   end

structure ByteSet =
   struct
      structure A = AtomicBitSet

      datatype t =
         N
       | C of Word8.word * A.t * t

      val empty = N

      fun isEmpty N = true
        | isEmpty (C _) = false

      (* TODO *)
      val universe =
         C (0w0 * A.bound, A.universe,
         C (0w1 * A.bound, A.universe,
         C (0w2 * A.bound, A.universe,
         C (0w3 * A.bound, A.universe,
         C (0w4 * A.bound, A.universe,
         N)))))

      fun insert' (N, base, offset) = C (base, A.singleton offset, N)
        | insert' (s as C (addr, ss, qs), base, offset) =
         if base < addr
            then C (base, A.singleton offset, s)
         else if base = addr
            then C (addr, A.insert (ss, offset), qs)
         else C (addr, ss, insert' (qs, base, offset))

      fun insert (s, w) =
         let
            val offset = Word8.mod (w, A.bound)
            val base = w - offset
         in
            insert' (s, base, offset)
         end

      fun singleton w = insert (N, w)

      fun remove' (N, _, _) = N
        | remove' (s as C (addr, ss, qs), base, offset) =
         if base < addr
            then s
         else if base = addr
            then
               let
                  val ss' = A.remove (ss, offset)
               in
                  if A.isEmpty ss'
                     then qs
                  else if A.same (ss, ss')
                     then s
                  else C (addr, ss', qs)
               end
         else C (addr, ss, remove' (qs, base, offset))

      fun remove (s, w) =
         let
            val offset = Word8.mod (w, A.bound)
            val base = w - offset
         in
            remove' (s, base, offset)
         end

      fun member' (N, _, _) = false
        | member' (C (addr, ss, qs), base, offset) =
         if base < addr
            then false
         else if base = addr
            then A.member (ss, offset)
         else member' (qs, base, offset)

      fun member (s, w) =
         let
            val offset = Word8.mod (w, A.bound)
            val base = w - offset
         in
            member' (s, base, offset)
         end

      fun union (N, s) = s
        | union (s, N) = s
        | union (s1 as C (addr1, ss1, qs1), s2 as C (addr2, ss2, qs2)) =
         case Word8.compare (addr1, addr2) of
            LESS => C (addr1, ss1, union (qs1, s2))
          | GREATER => C (addr2, ss2, union (s1, qs2))
          | EQUAL => C (addr1, A.union (ss1, ss2), union (qs1, qs2))

      fun intersect (N, _) = N
        | intersect (_, N) = N
        | intersect (s1 as C (addr1, ss1, qs1), s2 as C (addr2, ss2, qs2)) =
         case Word8.compare (addr1, addr2) of
            LESS => intersect (qs1, s2)
          | GREATER => intersect (s1, qs2)
          | EQUAL =>
               let
                  val ss = A.intersect (ss1, ss2)
                  val s = intersect (qs1, qs2)
               in
                  if A.isEmpty ss
                     then s
                  else C (addr1, ss, s)
               end

      fun difference (N, _) = N
        | difference (s1, N) = s1
        | difference (s1 as C (addr1, ss1, qs1), s2 as C (addr2, ss2, qs2)) =
         case Word8.compare (addr1, addr2) of
            LESS => C (addr1, ss1, difference (qs1, s2))
          | GREATER => difference (s1, qs2)
          | EQUAL =>
               let
                  val ss = A.difference (ss1, ss2)
                  val s = difference (qs1, qs2)
               in
                  if A.isEmpty ss
                     then s
                  else C (addr1, ss, s)
               end

      fun complement s = difference (universe, s)

      fun disjoint (N, _) = true
        | disjoint (_, N) = true
        | disjoint (s1 as C (addr1, ss1, qs1), s2 as C (addr2, ss2, qs2)) =
         case Word8.compare (addr1, addr2) of
            EQUAL => A.disjoint (ss1, ss2) andalso disjoint (qs1, qs2)
          | LESS => disjoint (qs1, s2)
          | GREATER => disjoint (s1, qs2)

      fun foldl f z N = z
        | foldl f z (C (addr, ss, qs)) =
         foldl f (A.foldl (fn (w, acc) => f (w + addr, acc)) z ss) qs

      fun compare (N, N) = EQUAL
        | compare (N, C _) = LESS
        | compare (C _, N) = GREATER
        | compare (C (a1, ss1, qs1), C (a2, ss2, qs2)) =
         case Word8.compare (a1, a2) of
            EQUAL =>
               (case A.compare (ss1, ss2) of
                   EQUAL => EQUAL
                 | order => order)
          | order => order

      fun first N = NONE
        | first (C (addr, ss, qs)) =
         SOME (addr + A.first ss)
   end

(* vim: set tw=0 ts=3 sw=3: *)
