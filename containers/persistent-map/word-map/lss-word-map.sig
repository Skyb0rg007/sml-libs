
signature LSS_WORD_MAP_STRUCTS =
   sig
      structure Value:
         sig
            type t

            val equals: t * t -> bool
         end

      (* Efficient Lazy Structure Sharing relies on a robust augment
       * The chosen augment should include some form of hash *)
      structure Aug:
         sig
            type t

            val zero: t
            val pure: word * Value.t -> t
            val + : t * t -> t
            val equals: t * t -> bool
         end
   end

signature LSS_WORD_MAP =
   sig
      include AUGMENTED_WORD_MAP

      (* Returns true if the maps are equal
       * If `shallowEq` returns false, nothing is known *)
      val shallowEq: map * map -> bool
   end

(* vim: set tw=0 ts=3 sw=3: *)
