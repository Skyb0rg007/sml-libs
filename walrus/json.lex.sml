
structure JSONLex =
struct

val SPACE: Word8.word = 0w2
val DQUOTE: Word8.word = 0w3
val PLUS: Word8.word = 0w4
val COMMA: Word8.word = 0w5
val MINUS: Word8.word = 0w6
val DOT: Word8.word = 0w7
val ZERO: Word8.word = 0w8
val DIGIT: Word8.word = 0w9
val COLON: Word8.word = 0w10
val XALPHA: Word8.word = 0w11
val E: Word8.word = 0w12
val LBRACK: Word8.word = 0w13
val BACKSLASH: Word8.word = 0w14
val RBRACK: Word8.word = 0w15
val a: Word8.word = 0w16
val b: Word8.word = 0w17
val e: Word8.word = 0w18
val f: Word8.word = 0w19
val f: Word8.word = 0w19

val ec = Array.array (256, 0w1: Word8.word)
val () = Array.update (ec, 0, 0w0)
val () = Array.update (ec, Char.ord #"\t", SPACE)
val () = Array.update (ec, Char.ord #"\n", SPACE)
val () = Array.update (ec, Char.ord #"\r", SPACE)
val () = Array.update (ec, Char.ord #" ", SPACE)
val () = Array.update (ec, Char.ord #"\"", DQUOTE)
val meta = Byte.stringToBytes ""
val chk = Byte.stringToBytes ""
val nxt = Byte.stringToBytes ""

end

(* vim: set tw=0 ts=3 sw=3: *)
