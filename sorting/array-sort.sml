
structure ArraySort :
sig
   val shellSort : ('a * 'a -> bool) -> 'a array -> unit
   val mergeSort : ('a * 'a -> bool) -> 'a array -> unit
   val heapSort : ('a * 'a -> bool) -> 'a array -> unit
   val radixSort : {buckets: int, rounds: int, radix: 'a * int -> int} -> 'a array -> unit
end =
struct

(* Marcin Ciura's gap sequence
 * https://web.archive.org/web/20180923235211/http://sun.aei.polsl.pl/~mciura/publikacje/shellsort.pdf
 *)
val gaps = Vector.fromList [701, 301, 132, 57, 23, 10, 4, 1]

fun shellSort gt arr =
   let
      val len = Array.length arr

      fun insertOrSwap (gap, i, elem) =
         if i < gap
            then Array.update (arr, i, elem)
         else
            let
               val nextIdx = i - gap
               val next = Array.sub (arr, nextIdx)
            in
               if gt (next, elem)
                  then
                     (Array.update (arr, i, next);
                      insertOrSwap (gap, nextIdx, elem))
               else Array.update (arr, i, elem)
            end

      (* Perform an insertion sort for the given gap sequence *)
      fun gappedInsertion gap =
         if gap >= len
            then ()
         else Array.appi (fn (i, x) => insertOrSwap (gap, i, x)) arr
   in
      Vector.app gappedInsertion gaps
   end

val cutoff = 5

fun mergeSort gt arr =
   let
      fun blit (src, srcPos, dst, dstPos, len) =
         ArraySlice.copy
         { src = ArraySlice.slice (src, srcPos, SOME len),
           dst = dst,
           di = dstPos }

      fun merge (src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) =
         let
            val src1r = src1ofs + src1len
            val src2r = src2ofs + src2len
            fun loop (i1, s1, i2, s2, d) =
               if not (gt (s1, s2))
                  then
                     let
                        val i1 = i1 + 1
                     in
                        Array.update (dst, d, s1);
                        if i1 < src1r
                           then loop (i1, Array.sub (arr, i1), i2, s2, d + 1)
                        else blit (src2, i2, dst, d + 1, src2r - i2)
                     end
               else
                  let
                     val i2 = i2 + 1
                  in
                     Array.update (dst, d, s2);
                     if i2 < src2r
                        then loop (i1, s1, i2, Array.sub (src2, i2), d + 1)
                     else blit (arr, i1, dst, d + 1, src1r - i1)
                  end
         in
            loop (src1ofs, Array.sub (arr, src1ofs),
                  src2ofs, Array.sub (src2, src2ofs),
                  dstofs)
         end

      fun isortto (srcofs, dst, dstofs, len) =
         let
            fun loop i =
               if i < len
                  then
                     let
                        val e = Array.sub (arr, srcofs + i)
                        val j = ref (dstofs + i - 1)
                     in
                        while (!j >= dstofs andalso gt (Array.sub (dst, !j), e)) do (
                           Array.update (dst, !j + 1, Array.sub (dst, !j));
                           j := !j - 1
                        );
                        Array.update (dst, !j + 1, e);
                        loop (i + 1)
                     end
               else ()
         in
            loop 0
         end

      fun sortto (srcofs, dst, dstofs, len) =
         if len <= cutoff
            then isortto (srcofs, dst, dstofs, len)
         else
            let
               val l1 = Int.div (len, 2)
               val l2 = len - l1
            in
               sortto (srcofs + l1, dst, dstofs + l1, l2);
               sortto (srcofs, arr, srcofs + l2, l1);
               merge (srcofs + l2, l1, dst, dstofs + l1, l2, dst, dstofs)
            end

      val len = Array.length arr
   in
      if len <= cutoff
         then isortto (0, arr, 0, len)
      else
         let
            val l1 = Int.div (len, 2)
            val l2 = len - l1
            val t = Array.array (l2, Array.sub (arr, 0))
         in
            sortto (l1, t, 0, l2);
            sortto (0, arr, l2, l1);
            merge (l2, l1, t, 0, l2, arr, 0)
         end
   end

exception Bottom of int

fun heapSort gt arr =
   let
      fun maxson (len, i) =
         let
            val i31 = i + i + i + 1
         in
            if i31 + 2 < len
               then
                  let
                     val x1 = Array.sub (arr, i31)
                     val x2 = Array.sub (arr, i31 + 1)
                     val x3 = Array.sub (arr, i31 + 2)
                  in
                     if gt (x2, x1)
                        then
                           if gt (x3, x2)
                              then i31 + 2
                           else i31 + 1
                     else if gt (x3, x1)
                        then i31 + 2
                     else i31
                  end
            else if i31 + 1 < len
               then
                  if gt (Array.sub (arr, i31 + 1), Array.sub (arr, i31))
                     then i31 + 1
                  else i31
            else if i31 < len
               then i31
            else raise Bottom i
         end

      fun trickleDown (len, i, e) =
         let
            val j = maxson (len, i)
         in
            if gt (Array.sub (arr, j), e)
               then (Array.update (arr, i, Array.sub (arr, j));
                     trickleDown (len, j, e))
            else Array.update (arr, i, e)
         end

      fun trickle (len, i, e) =
         trickleDown (len, i, e)
         handle Bottom i => Array.update (arr, i, e)

      fun bubbleDown (len, i) =
         let
            val j = maxson (len, i)
         in
            Array.update (arr, i, Array.sub (arr, j));
            bubbleDown (len, j)
         end

      fun bubble (len, i) =
         bubbleDown (len, i)
         handle Bottom i => i

      fun trickleUp (i, e) =
         let
            val father = Int.quot (i - 1, 3)
         in
            if gt (e, Array.sub (arr, father))
               then (Array.update (arr, i, Array.sub (arr, father));
                     if father > 0
                        then trickleUp (father, e)
                     else Array.update (arr, 0, e))
            else Array.update (arr, i, e)
         end

      val len = Array.length arr
   in
      raise Fail "NYI";
      if len > 1
         then
            let
               val e = Array.sub (arr, 1)
            in
               Array.update (arr, 1, Array.sub (arr, 0));
               Array.update (arr, 0, e)
            end
      else ()
   end

fun radixSort {buckets, rounds, radix} arr =
   if true then raise Fail "NYI" else

   if buckets < 1 orelse rounds < 1
      then raise Domain
   else if Array.length arr <= 1
      then ()
   else
      let
         val len = Array.length arr
         val dst = Array.array (len, Array.sub (arr, 0))
         val count = Array.array (buckets, 0)
         (* fun loop (src, dst, k) = *)
         (*    if k < passes *)
         (*       then () *)
      in
         ()
      end


local
   fun swap (arr, i, j) =
      let
         val temp = Array.sub (arr, i)
      in
         Array.update (arr, i, Array.sub (arr, j));
         Array.update (arr, j, temp)
      end

   (* Swap two (possibly non-consecutive) blocks with the same length *)
   fun blockSwap (arr, i, j, blockLen) =
      let
         fun loop offset =
            if offset >= blockLen
               then ()
            else
               (swap (arr, i + offset, j + offset)
               ; loop (offset + 1))
      in
         loop 0
      end

   (* Swap two consecutive blocks with possibly different lengths *)
   fun rotate (arr, start, leftLen, rightLen) =
      if leftLen <= 0 orelse rightLen <= 0
         then ()
      else if leftLen <= rightLen
         then
            (blockSwap (arr, start, start + leftLen, leftLen)
            ; rotate (arr, start + leftLen, leftLen, rightLen - leftLen))
      else
         (blockSwap (arr, start + leftLen - rightLen, start + leftLen, rightLen)
         ; rotate (arr, start, leftLen - rightLen, rightLen))

   fun insertSort (arr, start, len, cmp) =
      let
         fun inner (left, right) =
            if left >= start
                  andalso cmp (Array.sub (arr, left), Array.sub (arr, right)) = GREATER
               then
                  (swap (arr, left, right)
                  ; inner (left - 1, right - 1))
            else ()

         fun outer i =
            if i >= len
               then ()
            else
               let
                  val left = start + i - 1
                  val right = start + i
               in
                  inner (left, right);
                  outer (i + 1)
               end
      in
         outer 1
      end

   fun binarySearchLeft (arr, start, len, target, cmp) =
      let
         fun loop (left, right) =
            if left >= right
               then left
            else
               let
                  val mid = left + Int.quot (right - left, 2)
               in
                  if cmp (target, Array.sub (arr, start + mid)) = GREATER
                     then loop (mid + 1, right)
                  else loop (left, mid)
               end
      in
         loop (0, len)
      end

   fun binarySearchRight (arr, start, len, target, cmp) =
      let
         fun loop (left, right) =
            if left >= right
               then right
            else
               let
                  val mid = left + Int.quot (right - left, 2)
               in
                  if cmp (Array.sub (arr, start + mid), target) = GREATER
                     then loop (left, mid)
                  else loop (mid + 1, mid)
               end
      in
         loop (0, len)
      end

   fun collectKeys (arr, start, len, idealKeys, cmp) =
      let
         fun loop (keysFound, firstKey, curKey) =
            if curKey >= len orelse keysFound >= idealKeys
               then
                  (rotate (arr, start, firstKey, keysFound)
                  ; keysFound)
            else
               let
                  val insertPos = binarySearchLeft
                     (arr, start + firstKey, keysFound,
                      Array.sub (arr, start + curKey), cmp)
               in
                  if insertPos = keysFound
                        orelse cmp (Array.sub (arr, start + curKey),
                                    Array.sub (arr, start + firstKey + insertPos))
                               <> EQUAL
                     then
                        (rotate (arr, start + firstKey, keysFound, curKey - (firstKey + keysFound))
                        ; rotate (arr, start + curKey - keysFound + insertPos, keysFound - insertPos, 1)
                        ; loop (keysFound + 1, curKey - keysFound, curKey + 1))
                  else loop (keysFound, firstKey, curKey + 1)
               end
      in
         loop (1, 0, 1)
      end
in
   fun grailSort cmp arr =
      raise Fail "NYI"
end

end

(* vim: set tw=0 ts=3 sw=3: *)
