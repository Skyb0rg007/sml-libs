
signature HASH_CONS_WORD_MAP_STRUCTS =
sig

structure Value:
   sig
      type t

      val equals: t * t -> bool
      val hash: t -> word
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
