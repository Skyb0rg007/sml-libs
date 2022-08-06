
structure Util =
struct
   open Compat

   structure Vector =
   struct
      open Vector

      fun insert (v, idx, x) =
         let
            fun gen i =
               case Int.compare (i, idx) of
                  LESS => sub (v, i)
                | EQUAL => x
                | GREATER => sub (v, i - 1)
         in
            tabulate (length v + 1, gen)
         end

      fun remove (v, idx) =
         let
            fun gen i =
               if i < idx
                  then sub (v, i)
               else sub (v, i + 1)
         in
            tabulate (length v - 1, gen)
         end
   end

   structure Word32 =
   struct
      open Word32

      fun trailingZeros 0w0 = 32
        | trailingZeros w =
         let
            fun go (mask, set, w, n) =
               if andb (w, mask) = 0w0
                  then (>> (w, set), Word.+ (n, set))
               else (w, n)

            val (w, n) = go (0wx0000ffff, 0w16, w, 0w0)
            val (w, n) = go (0wx000000ff, 0w8, w, n)
            val (w, n) = go (0wx0000000f, 0w4, w, n)
            val (w, n) = go (0wx00000003, 0w2, w, n)
            val (w, n) = go (0wx00000001, 0w1, w, n)
         in
            Word.toInt n
         end

      fun leadingZeros 0w0 = 32
        | leadingZeros w =
         let
            fun go (mask, set, w, n) =
               if andb (w, mask) = 0w0
                  then (<< (w, set), Word.+ (n, set))
               else (w, n)

            val (w, n) = go (0wxffff0000, 0w16, w, 0w0)
            val (w, n) = go (0wxff000000, 0w8, w, 0w0)
            val (w, n) = go (0wxf0000000, 0w4, w, 0w0)
            val (w, n) = go (0wxc0000000, 0w2, w, 0w0)
            val (w, n) = go (0wx80000000, 0w1, w, 0w0)
         in
            Word.toInt n
         end
   end
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
