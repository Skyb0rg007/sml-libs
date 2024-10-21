
signature PICKLE =
sig

type 'a t

(*** Primitive Types ***)

(* BOOL *)
val bool : Bool.bool t

(* INTEGER *)
val int : Int.int t
(* val int32 : Int32.int t *)
(* val int64 : Int64.int t *)
(* val intInf : IntInf.int t *)
(* val fixedInt : FixedInt.int t *)
val largeInt : LargeInt.int t
val position : Position.int t

(* WORD *)
val word : Word.word t
val word8 : Word8.word t
(* val word32 : Word32.word t *)
(* val word64 : Word64.word t *)
val largeWord : LargeWord.word t
(* val sysWord : SysWord.word t *)

(* REAL *)
val real : Real.real t
val largeReal : LargeReal.real t
(* val real32 : Real32.real t *)
(* val real64 : Real64.real t *)

(* CHAR *)
val char : Char.char t

(*** Monomorphic Containers ***)

(* STRING, SUBSTRING *)
val string : String.string t
val substring : Substring.string t

(* MONO_VECTOR, MONO_ARRAY, MONO_VECTOR_SLICE, MONO_ARRAY_SLICE *)
val charVector : CharVector.vector t
val charArray : CharArray.array t
val charVectorSlice : CharVectorSlice.slice t
val charArraySlice : CharArraySlice.slice t
val word8Vector : Word8Vector.vector t
val word8VectorSlice : Word8VectorSlice.slice t
val word8Array : Word8Array.array t
val word8ArraySlice : Word8ArraySlice.slice t

(*** Polymorphic Containers ***)

(* Variable-sized sequence *)
val list : 'a t -> 'a list t
val vector : 'a t -> 'a Vector.vector t
val vectorSlice : 'a t -> 'a VectorSlice.slice t
val array : 'a t -> 'a Array.array t
val arraySlice : 'a t -> 'a ArraySlice.slice t

(* Option *)
val option : 'a t -> 'a option t

(*** Datatypes ***)

val enum : string list -> string t
val data : string -> (string * 'a t) list -> 'a t
val mu : ('a t -> 'a t) -> 'a t

(*** Pseudo-Containers ***)

(* val set : 'v t -> 'v list t *)
(* val map : 'k t * 'v t -> ('k * 'v) list t *)

(*** Miscellaneous Types ***)

(* DATE *)
(* val weekday : Date.weekday t *)
(* val month : Date.month t *)
(* val date : Date.date t *)

(* TIME *)
(* val time : Time.time t *)

end

