
signature GENERIC_WORD_MAP_STRUCTS =
sig

structure Word:
   sig
      include WORD

      val equals: word * word -> bool
      val highestBitMask: word -> word
   end

structure Value:
   sig
      type t

      val equals: t * t -> bool
   end

structure Aug:
   sig
      type t

      val zero: t
      val pure: Word.word * Value.t -> t
      val + : t * t -> t
      val equals: t * t -> bool
   end

structure Ref:
   sig
      type 'a t

      val caches: bool
      val make: 'a -> 'a t
      val ! : 'a t -> 'a
      val liftEquals: ('a * 'a -> bool) -> 'a t * 'a t -> bool
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
