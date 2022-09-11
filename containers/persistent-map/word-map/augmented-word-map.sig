
signature AUGMENTED_WORD_MAP_STRUCTS =
   sig
      structure Value:
         sig
            type t

            val equals: t * t -> bool
         end

      structure Aug:
         sig
            type t

            val zero: t
            val pure: word * Value.t -> t
            val + : t * t -> t
            val equals: t * t -> bool
         end
   end

signature AUGMENTED_WORD_MAP =
   sig
      include MONO_MAP where type key = word

      type augment

      val augment: map -> augment
      val viewMin: map -> (key * value * map) option
      val viewMax: map -> (key * value * map) option
      val isSubmap: map * map -> bool
      val equals: map * map -> bool
   end

(* vim: set tw=0 ts=3 sw=3: *)
