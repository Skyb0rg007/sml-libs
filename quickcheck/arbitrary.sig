
signature ARBITRARY =
sig

structure Gen:
   sig
      type 'a t

      val pure: 'a -> 'a t
      val map: ('a -> 'b) -> 'a t -> 'b t
      val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
      val map3: ('a * 'b * 'c -> 'd) -> 'a t * 'b t * 'c t -> 'd t
      val bind: 'a t -> ('a -> 'b t) -> 'b t
      val sequence: 'a t list -> 'a list t
      val fix: (('a -> 'b t) -> 'a -> 'b t) -> 'a -> 'b t
      val delay: ('a t -> 'a) t

      (* Generating random functions *)
      val promote: ('a -> 'b t) -> ('a -> 'b) t
      val variant: IntInf.int * 'a t -> 'a t

      (* Size parameter *)
      val sized: (int -> 'a t) -> 'a t
      val resize: int * 'a t -> 'a t
      val scale: (int -> int) -> 'a t -> 'a t

      (* Primitive choice-based generators *)
      val chooseInt: int * int -> int t
      val chooseWord: word * word -> word t
      val chooseWord64: Word64.word * Word64.word -> Word64.word t
      val chooseIntInf: IntInf.int * IntInf.int -> IntInf.int t

      (* Combinators *)
      val suchThat: ('a -> bool) -> 'a t -> 'a t
      val suchThatOpt: ('a -> bool) -> 'a t -> 'a option t
      val suchThatMap: ('a -> 'b option) -> 'a t -> 'b t
      val oneof: 'a t list -> 'a t
      val elements: 'a list -> 'a t
      val frequency: (int * 'a t) list -> 'a t
      val listOf: 'a t -> 'a list t
      val listOfSize: int * 'a t -> 'a list t

      (* Primitive type-based generators *)
      val int: int t
      val word: word t

      val coInt: int * 'a t -> 'a t
      val coWord: word * 'a t -> 'a t
      val coBool: bool * 'a t -> 'a t
      val coOption: ('a * 'b t -> 'b t) -> 'a option * 'b t -> 'b t

      (* Running a generator *)
      val generate: SplitMix.t * 'a t -> 'a
      val sample: SplitMix.t * 'a t -> 'a list
   end

structure Shrink:
   sig
      type 'a t = 'a -> 'a Seq.t

      val shrinkOption: 'a t -> 'a option t
      val shrinkList: 'a t -> 'a list t
      val shrinkInt: int t
      val shrinkReal: real t
      val shrinkNothing: 'a t
   end

structure Property:
   sig
      type t
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
