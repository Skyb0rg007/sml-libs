
signature WORD_EXTRA =
sig

(* `highestBitMask w` returns a word where the only set bit is the highest bit of `w`
 * Equivalent to `1 << (wordSize - 1 - leadingZeros w)`,
 * but can be more efficient (since leadingZeros is not an SML primitive)
 * Result is unspecified when given 0w0 *)
val highestBitMask: word -> word

(* `trailingZeros w` is the number of zeros after the first set bit
 * When given 0w0, should return `Word.wordSize` *)
val trailingZeros: word -> int

(* `popCount w` is the number of set bits in `w` *)
val popCount: word -> int

end

(* vim: set tw=0 ts=3 sw=3: *)
