
structure CBOR =
struct

structure X = struct

datatype value
  = ARRAY of value list (* Major type 4 *)
  | BOOL of bool        (* Major type 7 - simple values 20 and 21 *)
  | INT of IntInf.int   (* Major types 0,1,6,7 based on type *)
  | FLOAT of real       (* Major type 7 - encoded as 64,32,16 bit IEEE754 *)
  | BYTESTRING of Word8Vector.vector (* Major type 2 *)
  | STRING of string (* Major type 3 - must be valid UTF-8 *)
  | MAP of (value * value) list
  | TAG of Word64.word * value

end

structure W8 = Word8
structure W64 = Word64

(* Helper functions *)
fun outputWord8 (stream, w) =
  BinIO.output1 (stream, W8.fromLarge (W64.toLarge w))

fun outputWord16BE (stream, w) =
  (outputWord8 (stream, W64.>> (w, 0w8));
   outputWord8 (stream, w))

fun outputWord32BE (stream, w) =
  (outputWord16BE (stream, w);
   outputWord16BE (stream, W64.>> (w, 0w16)))

fun outputWord64BE (stream, w) =
  (outputWord32BE (stream, w);
   outputWord32BE (stream, W64.>> (w, 0w32)))

fun outputReal64BE (stream, r) =
  outputWord64BE (stream, Unsafe.realToBits r)

(* Write the header, given a type tag and payload *)
fun header (stream, ttag, w) =
  if w < 0w24 then
    BinIO.output1 (stream, W8.orb (ttag, W8.fromLarge (W64.toLarge w)))
  else if w <= 0wxff then
    (BinIO.output1 (stream, W8.orb (ttag, 0w24)); outputWord8 (stream, w))
  else if w <= 0wxffff then
    (BinIO.output1 (stream, W8.orb (ttag, 0w25)); outputWord16BE (stream, w))
  else if w <= 0wxffffffff then
    (BinIO.output1 (stream, W8.orb (ttag, 0w26)); outputWord32BE (stream, w))
  else (BinIO.output1 (stream, W8.orb (ttag, 0w27)); outputWord64BE (stream, w))

val POS_INT : W8.word = 0wx00
val NEG_INT : W8.word = 0wx20
val BYTESTR : W8.word = 0wx40
val TEXTSTR : W8.word = 0wx60
val ARRAY   : W8.word = 0wx80
val MAP     : W8.word = 0wxa0
val TAG     : W8.word = 0wxc0
val SIMPLE  : W8.word = 0wxe0

(* Major Type 0: positive integer *)
fun posInt (stream, w) = header (stream, POS_INT, w)

(* Major Type 1: negative integer *)
fun negInt (stream, w) = header (stream, NEG_INT, w)

(* Major Type 2: bytestring *)
fun bytestring (stream, bytes) =
  (header (stream, BYTESTR, W64.fromInt (Word8Vector.length bytes));
   BinIO.output (stream, bytes))

(* Major Type 3: UTF-8 string *)
fun string (stream, string) =
  (header (stream, TEXTSTR, W64.fromInt (String.size string));
   BinIO.output (stream, Byte.stringToBytes string))

(* Major Type 4: array *)
fun beginArray (stream, len) = header (stream, ARRAY, W64.fromInt len)

(* Major Type 5: maps *)
fun beginMap (stream, len) = header (stream, MAP, W64.fromInt len)

(* Major Type 6: tag *)
fun tag (stream, w) = header (stream, TAG, w)

(* Converts an IntInf.int to Word8Vector.vector, in network (big endian) order
 * This encodes 0 as the empty vector *)
fun bignumToBytes n =
  if n < 0 then raise Domain else
    let
      val len = if n = 0 then 0 else Int.quot (IntInf.log2 n + 8, 8)
      fun gen i =
        W8.fromLargeInt (IntInf.~>> (n, 0w8 * Word.fromInt (len - i - 1)))
    in
      Word8Vector.tabulate (len, gen)
    end

fun posBignum (stream, n) =
  (tag (stream, 0w2);
   bytestring (stream, bignumToBytes n))

fun negBignum (stream, n) =
  (tag (stream, 0w3);
   bytestring (stream, bignumToBytes n))

(* Major Type 7: simple *)
fun simple (stream, w) = header (stream, SIMPLE, w)

fun bool (stream, b) = simple (stream, if b then 0w21 else 0w20)

fun null stream = simple (stream, 0w22)

fun undefined stream = simple (stream, 0w23)

(* fun real16 (stream, r) = (simple (stream, 0w25); outputReal16BE (stream, r)) *)
(* fun real32 (stream, r) = (simple (stream, 0w26); outputReal32BE (stream, r)) *)

fun real64 (stream, r) = (simple (stream, 0w27); outputReal64BE (stream, r))

end
