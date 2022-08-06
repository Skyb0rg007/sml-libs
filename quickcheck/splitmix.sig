
signature SPLITMIX =
sig

type t

(* Creation. `new` uses `Time.toMilliseconds` for randomness *)
val new: unit -> t
val fromSeed: Word64.word -> t

(* Namesake function. Produces two uncorrelated states *)
val split: t -> t * t

(* Uniform over entire range *)
val nextInt: t -> int * t
val nextWord: t -> word * t
val nextWord64: t -> Word64.word * t
val nextReal: t -> real * t

(* Uniform in given range *)
val chooseInt: int * int -> t -> int * t
val chooseWord: word * word -> t -> word * t
val chooseWord64: Word64.word * Word64.word -> t -> Word64.word * t
val chooseIntInf: IntInf.int * IntInf.int -> t -> IntInf.int * t

val toString: t -> string

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
