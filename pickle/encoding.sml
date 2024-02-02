
signature JSON_ENCODE =
sig

type json

datatype error =
   (* JSON only handles unicode data *)
   InvalidUtf8
   (* JSON cannot encode NaN or Infinity *)
 | InvalidReal
   (* [Strict] Integer is out of the portable range *)
 | InvalidInt
   (* [Strict] Object has duplicate field names *)
 | DuplicateField of string

exception EncodingError of error

val null : json
val true_ : json
val false_ : json
val bool : bool -> json
val int : int -> json
val real : real -> json
val string : string -> json
val substring : substring -> json
val array : json list -> json
val object : (string * json) list -> json
val canonObject : (string * json) list -> json

val toString : json -> string
val toByteArray : json -> Word8Array.array
val size : json -> int

end

structure Encoding : JSON_ENCODE =
struct

(* TODO: include more information *)
datatype error =
   InvalidUtf8
 | InvalidReal
 | InvalidInt
 | DuplicateField of string

exception EncodingError of error

(* Calculate the length of the JSON-encoded substring
 * Raises an exception on invalid UTF8 *)
(* TODO: Some checks may be redundant *)
fun utf8len' (s, start, stop) =
   let
      fun assert true = ()
        | assert false = raise EncodingError InvalidUtf8

      (* Checks for a UTF8 continuation byte *)
      fun assertCont w =
         assert (Word8.andb (w, 0wxC0) = 0wx80)

      (* Computes ((w & mask) | (cp << 6))
       * Used for building Unicode codepoints *)
      fun build (w, mask, cp) =
         Word32.orb
         (Word32.fromLarge (Word8.toLarge (Word8.andb (w, mask))),
          Word32.<< (cp, 0w6))

      fun go (i, acc) =
         if i >= stop
            then acc
         else
            let
               val w1 = Byte.charToByte (String.sub (s, i))
            in
               (* ASCII - no validation needed *)
               if w1 < 0wx20
                  then
                     (* \b, \t, \n, \f, \r *)
                     if w1 = 0wx8 orelse w1 = 0wx9 orelse w1 = 0wxA orelse w1 = 0wxC orelse w1 = 0wxD
                        then go (i + 1, acc + 2)
                     else go (i + 1, acc + 6) (* \uXXXX *)
               else if w1 < 0wx80
                  then
                     (* \", \\ *)
                     if w1 = 0wx22 orelse w1 = 0wx5C
                        then go (i + 1, acc + 2)
                     else go (i + 1, acc + 1)
               (* 2-byte UTF8 *)
               else if Word8.andb (w1, 0wxE0) = 0wxC0
                  then
                     let
                        val () = assert (i + 1 < stop)
                        val w2 = Byte.charToByte (String.sub (s, i + 1))
                        val () = assertCont w2
                        val cp = build (w2, 0wx1F, build (w1, 0wx3F, 0w0))
                     in
                        (* XXX: is this check necessary? *)
                        assert (0wx80 <= cp andalso cp <= 0wx7FF);
                        go (i + 2, acc + 2)
                     end
               (* 3-byte UTF8 *)
               else if w1 < 0wxF0
                  then
                     let
                        val () = assert (i + 2 < stop)
                        val w2 = Byte.charToByte (String.sub (s, i + 1))
                        val w3 = Byte.charToByte (String.sub (s, i + 2))
                        val () = assertCont w2
                        val () = assertCont w3
                        val cp = build (w3, 0wx3F, build (w2, 0wx3F, build (w1, 0wxF, 0w0)))
                     in
                        assert (0wx800 <= cp andalso cp <= 0wxFFFF);
                        assert (cp < 0wxD800 orelse 0wxDFFF < cp);
                        go (i + 3, acc + 3)
                     end
               (* 4-byte UTF8 *)
               else
                  let
                     val () = assert (i + 3 < stop)
                     val w2 = Byte.charToByte (String.sub (s, i + 1))
                     val w3 = Byte.charToByte (String.sub (s, i + 2))
                     val w4 = Byte.charToByte (String.sub (s, i + 3))
                     val () = assertCont w2
                     val () = assertCont w3
                     val () = assertCont w4
                     val cp = build (w4, 0wx3F, build (w3, 0wx3F, build (w2, 0wx3F, build (w1, 0wx7, 0w0))))
                  in
                     assert (0wx10000 <= cp andalso cp <= 0wx10FFFF);
                     go (i + 4, acc + 4)
                  end
            end
   in
      go (start, 0)
   end

fun utf8len s = utf8len' (s, 0, String.size s)

val digits = Byte.stringToBytes "0123456789abcdef"

(* Perform the writing. We already know the string is valid UTF8. *)
fun writeString' (s, start, stop) (a, i) =
   let
      fun go (idx, i) =
         if idx >= stop
            then i
         else
            let
               val x = Byte.charToByte (String.sub (s, idx))
            in
               if x < 0wx80
                  then
                     if x = 0wx08
                        then (Word8Array.update (a, i, 0wx5C (* \ *));
                              Word8Array.update (a, i + 1, 0wx62 (* b *));
                              go (idx + 1, i + 2))
                     else if x = 0wx09
                        then (Word8Array.update (a, i, 0wx5C (* \ *));
                              Word8Array.update (a, i + 1, 0wx74 (* t *));
                              go (idx + 1, i + 2))
                     else if x = 0wx0A
                        then (Word8Array.update (a, i, 0wx5C (* \ *));
                              Word8Array.update (a, i + 1, 0wx6E (* n *));
                              go (idx + 1, i + 2))
                     else if x = 0wx0C
                        then (Word8Array.update (a, i, 0wx5C (* \ *));
                              Word8Array.update (a, i + 1, 0wx66 (* f *));
                              go (idx + 1, i + 2))
                     else if x = 0wx0D
                        then (Word8Array.update (a, i, 0wx5C (* \ *));
                              Word8Array.update (a, i + 1, 0wx72 (* r *));
                              go (idx + 1, i + 2))
                     else if x = 0wx22
                        then (Word8Array.update (a, i, 0wx5C (* \ *));
                              Word8Array.update (a, i + 1, 0wx22 (* " *));
                              go (idx + 1, i + 2))
                     else if x = 0wx5C
                        then (Word8Array.update (a, i, 0wx5C (* \ *));
                              Word8Array.update (a, i + 1, 0wx5C (* \ *));
                              go (idx + 1, i + 2))
                     else if x < 0wx20
                        then 
                           (Word8Array.update (a, i, 0wx5C (* \ *));
                            Word8Array.update (a, i + 1, 0wx75 (* u *));
                            Word8Array.update (a, i + 2, Word8Vector.sub (digits, Word8.toInt (Word8.andb (Word8.>> (x, 0w12), 0wxF))));
                            Word8Array.update (a, i + 3, Word8Vector.sub (digits, Word8.toInt (Word8.andb (Word8.>> (x, 0w8), 0wxF))));
                            Word8Array.update (a, i + 4, Word8Vector.sub (digits, Word8.toInt (Word8.andb (Word8.>> (x, 0w4), 0wxF))));
                            Word8Array.update (a, i + 5, Word8Vector.sub (digits, Word8.toInt (Word8.andb (x, 0wxF))));
                            go (idx + 1, i + 6))
                     else (Word8Array.update (a, i, x); go (idx + 1, i + 1))
               else if x < 0wxE0
                  then
                     (Byte.packString (a, i, Substring.substring (s, idx, 2));
                      go (idx + 2, i + 2))
               else if x < 0wxF0
                  then
                     (Byte.packString (a, i, Substring.substring (s, idx, 3));
                      go (idx + 3, i + 3))
               else
                  (Byte.packString (a, i, Substring.substring (s, idx, 4));
                   go (idx + 4, i + 4))
            end

      val () = Word8Array.update (a, i, 0wx22 (* " *))
      val i = go (start, i + 1)
   in
      Word8Array.update (a, i, 0wx22 (* " *));
      i + 1
   end

fun writeString s = writeString' (s, 0, String.size s)

(* Combination of the size (in bytes) of the encoded JSON,
 * and a function that writes those bytes into the array at the given index.
 * The writing function then returns the next unwritten index. *)
datatype json = JSON of int * (Word8Array.array * int -> int)

fun size (JSON (n, _)) = n

fun toByteArray (JSON (size, poke)) =
   let
      val arr = Word8Array.array (size, 0w0)
   in
      ignore (poke (arr, 0));
      arr
   end

fun toString j = Byte.unpackString (Word8ArraySlice.full (toByteArray j))

local
   fun literal s =
      let
         val ss = Substring.full s
         val size = String.size s
      in
         JSON (String.size s, fn (a, i) => (Byte.packString (a, i, ss); i + size))
      end
in
   val null = literal "null"
   val true_ = literal "true"
   val false_ = literal "false"
   val emptyArray = literal "[]"
   val emptyObject = literal "{}"

   (* TODO: check for portability concerns *)
   fun int n =
      if n < 0
         then literal ("-" ^ Int.toString (~n))
      else literal (Int.toString n)

   fun real n =
      if Real.isFinite n
         then literal (Ryu.toString n)
      else raise EncodingError InvalidReal
end

fun bool true = true_
  | bool false = false_

fun string s = JSON (utf8len s + 2, writeString s)

fun substring s =
   let
      val b = Substring.base s
   in
      JSON (utf8len' b + 2, writeString' b)
   end

fun array [] = emptyArray
  | array xs =
   let
      val (count, elemSize) =
         List.foldl
            (fn (JSON (size, _), (count, elemSize)) =>
               (count + 1, elemSize + size))
            (0, 0)
            xs
      fun poke (a, i) =
         let
            val () = Word8Array.update (a, i, 0wx5B (* [ *))
            val (_, i) =
               List.foldl
                  (fn (JSON (_, poke), (first, i)) =>
                     if first
                        then (false, poke (a, i))
                     else
                        (Word8Array.update (a, i, 0wx2C (* , *));
                         (false, poke (a, i + 1))))
                  (true, i + 1)
                  xs
         in
            Word8Array.update (a, i, 0wx5d (* ] *));
            i + 1
         end
      val size = 2 + Int.max (count - 1, 0) + elemSize
   in
      JSON (size, poke)
   end

fun object [] = emptyObject
  | object kvs =
   let
      val (count, contentSize) =
         List.foldl
            (fn ((key, JSON (size, _)), (count, contentSize)) =>
               (count + 1, contentSize + utf8len key + size))
            (0, 0)
            kvs
      val size = 2 + Int.max (count - 1, 0) + count * 3  + contentSize
      fun row (key, valuePoke, a, i) =
         let
            val i = writeString key (a, i)
            val () = Word8Array.update (a, i, 0wx3A (* : *))
         in
            valuePoke (a, i + 1)
         end
      fun poke (a, i) =
         let
            val () = Word8Array.update (a, i, 0wx7B (* { *))
            val (_, i) =
               List.foldl
                  (fn ((key, JSON (_, poke)), (first, i)) =>
                     if first
                        then (false, row (key, poke, a, i))
                     else (Word8Array.update (a, i, 0wx2C (* , *));
                           (false, row (key, poke, a, i + 1))))
                  (true, i + 1)
                  kvs
         in
            Word8Array.update (a, i, 0wx7D (* } *));
            i + 1
         end
   in
      JSON (size, poke)
   end

(* Compare the UTF-8 encoded strings as if they were arrays of UTF-16 code units
 * https://icu-project.org/docs/papers/utf16_code_point_order.html#utf-8-in-utf-16-order *)
fun compareUtf16 (s1, s2) =
   let
      val size1 = String.size s1
      val size2 = String.size s2
      fun loop i =
         case (i < size1, i < size2) of
            (false, false) => EQUAL
          | (true, false) => GREATER
          | (false, true) => LESS
          | (true, true) =>
            let
               val w1 = Byte.charToByte (String.sub (s1, i))
               val w2 = Byte.charToByte (String.sub (s2, i))
               fun fixup w =
                  if Word8.andb (w, 0wxFE) = 0wxEE
                     then w + 0wx10
                  else w
            in
               if w1 = w2
                  then loop (i + 1)
               else if w1 >= 0wxEE andalso w2 >= 0wxEE
                  then
                     if fixup w1 < fixup w2
                        then LESS
                     else GREATER
               else if w1 < w2
                  then LESS
               else GREATER
            end
   in
      loop 0
   end

local
   (* OCaml's list-based merge-sort algorithm *)
   fun lt ((k1, _), (k2, _)) =
      case compareUtf16 (k1, k2) of
         LESS => true
       | GREATER => false
       | EQUAL => raise EncodingError (DuplicateField k1)

   fun gt (a, b) = lt (b, a)

   fun revAppend ([], l2) = l2
     | revAppend (h :: t, l2) = revAppend (t, h :: l2)

   fun revMerge ([], l2, acc) = revAppend (l2, acc)
     | revMerge (l1, [], acc) = revAppend (l1, acc)
     | revMerge (l1 as h1 :: t1, l2 as h2 :: t2, acc) =
      if lt (h1, h2)
         then revMerge (t1, l2, h1 :: acc)
      else revMerge (l1, t2, h2 :: acc)

   fun revMergeRev ([], l2, acc) = revAppend (l2, acc)
     | revMergeRev (l1, [], acc) = revAppend (l1, acc)
     | revMergeRev (l1 as h1 :: t1, l2 as h2 :: t2, acc) =
      if gt (h1, h2)
         then revMergeRev (t1, l2, h1 :: acc)
      else revMergeRev (l1, t2, h2 :: acc)

   fun sort (2, x1 :: x2 :: tl) =
      if lt (x1, x2)
         then ([x1, x2], tl)
      else ([x2, x1], tl)
     | sort (3, x1 :: x2 :: x3 :: tl) =
      if lt (x1, x2)
         then
            if lt (x2, x3)
               then ([x1, x2, x3], tl)
            else if lt (x1, x3)
               then ([x1, x3, x2], tl)
            else ([x3, x1, x2], tl)
      else if lt (x1, x3)
         then ([x2, x1, x3], tl)
      else if lt (x2, x3)
         then ([x2, x3, x1], tl)
      else ([x3, x2, x1], tl)
     | sort (n, l) =
      let
         val n1 = Int.quot (n, 2)
         val n2 = n - n1
         val (s1, l2) = revSort (n1, l)
         val (s2, tl) = revSort (n2, l2)
      in
         (revMergeRev (s1, s2, []), tl)
      end

   and revSort (2, x1 :: x2 :: tl) =
      if gt (x1, x2)
         then ([x1, x2], tl)
      else ([x2, x1], tl)
     | revSort (3, x1 :: x2 :: x3 :: tl) =
      if gt (x1, x2)
         then
            if gt (x2, x3)
               then ([x1, x2, x3], tl)
            else if gt (x1, x3)
               then ([x1, x3, x2], tl)
            else ([x3, x1, x2], tl)
      else if gt (x1, x3)
         then ([x2, x1, x3], tl)
      else if gt (x2, x3)
         then ([x2, x3, x1], tl)
      else ([x3, x2, x1], tl)
     | revSort (n, l) =
      let
         val n1 = Int.quot (n, 2)
         val n2 = n - n1
         val (s1, l2) = sort (n1, l)
         val (s2, tl) = sort (n2, l2)
      in
         (revMerge (s1, s2, []), tl)
      end

   val sort = fn l =>
      let
         val len = List.length l
      in
         if len < 2
            then l
         else #1 (sort (len, l))
      end
in
   fun canonObject kvs = object (sort kvs)
end

end

(* vim: set tw=0 ts=3 sw=3: *)
