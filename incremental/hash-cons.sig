
signature HASH_CONS_TYPE =
sig
   type t

   val hash: t -> word
   val same: t * t -> bool
end

signature HASH_CONS =
sig
end
(* vim: set tw=0 ts=3 sw=3: *)
