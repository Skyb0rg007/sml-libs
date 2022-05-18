(* Implements a splittable RNG
 * Used in Java and Haskell
 * Note: This is not cryptographically secure!
 * Ported from: https://hackage.haskell.org/package/splitmix-0.1.0.4
 * Original Paper: https://dl.acm.org/doi/10.1145/2714064.2660195
 *)

signature SPLITMIX =
sig
   type t

   (* Helper to create a seed from the current time (in seconds)
    * It is recommended to only use this once since it only uses seconds
    * Use `split` for more RNGs instead *)
   val initialSeed: unit -> Word64.word

   (* Create from an initial seed *)
   val fromSeed: Word64.word -> t

   (* Split generator into two unrelated generators *)
   val split: t -> t * t

   (* Random word in range [0, 2^64) *)
   val nextWord64: t -> Word64.word * t

   (* Random int in range [-2^63, 2^63) *)
   val nextInt64: t -> Int64.int * t

   (* Random int in `int`'s range, or [-2^63, 2^63) if infinite *)
   val nextInt: t -> int * t

  (* Random number in range [0, 1) *)
   val nextReal: t -> real * t

   (* `nextIntInf (lo, hi, g)`: Random number between `lo` and `hi` *)
   val nextIntInf: IntInf.int * IntInf.int * t -> IntInf.int * t
end

(* vim: set tw=0 ts=3 sw=3: *)
