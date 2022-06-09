
signature CONDUIT =
sig
   type ('l, 'i, 'o, 'u, 'r) pipe
   type ('i, 'o, 'r) t = ('i, 'i, 'o, unit, 'r) pipe

   val pure: 'r -> ('l, 'i, 'o, 'u, 'r) pipe
   val map: ('a -> 'b) -> ('l, 'i, 'o, 'u, 'a) pipe -> ('l, 'i, 'o, 'u, 'b) pipe
   val >>= : ('l, 'i, 'o, 'u, 'a) pipe * ('a -> ('l, 'i, 'o, 'u, 'b) pipe) -> ('l, 'i, 'o, 'u, 'b) pipe
   val =<< : ('a -> ('l, 'i, 'o, 'u, 'b) pipe) * ('l, 'i, 'o, 'u, 'a) pipe -> ('l, 'i, 'o, 'u, 'b) pipe
   val >=> : ('a -> ('l, 'i, 'o, 'u, 'b) pipe) * ('b -> ('l, 'i, 'o, 'u, 'c) pipe) -> 'a -> ('l, 'i, 'o, 'u, 'c) pipe
   val <=< : ('b -> ('l, 'i, 'o, 'u, 'c) pipe) * ('a -> ('l, 'i, 'o, 'u, 'b) pipe) -> 'a -> ('l, 'i, 'o, 'u, 'c) pipe
   val ap: ('l, 'i, 'o, 'u, 'a -> 'b) pipe * ('l, 'i, 'o, 'u, 'a) pipe -> ('l, 'i, 'o, 'u, 'b) pipe

   val await: ('l, 'i, 'o, 'u, 'i option) pipe
   val awaitE: ('l, 'i, 'o, 'u, ('u, 'i) Either.either) pipe
   val awaitForever: ('i -> ('l, 'i, 'o, 'r, 'r') pipe) -> ('l, 'i, 'o, 'r, 'r) pipe
   val yield: 'o -> ('l, 'i, 'o, 'u, unit) pipe
   val leftover: 'l -> ('l, 'i, 'o, 'u, unit) pipe
   val uncons: (Void.t, unit, 'o, unit , unit) pipe -> ('o * (Void.t, unit, 'o, unit, unit) pipe) option
   val unconsE: (Void.t, unit, 'o, unit , 'r) pipe -> ('r, 'o * (Void.t, unit, 'o, unit, 'r) pipe) Either.either

   val id: unit -> ('l, 'a, 'a, 'r, 'r) pipe

   val pipe: ('l, 'a, 'b, 'r0, 'r1) pipe * (Void.t, 'b, 'c, 'r1, 'r2) pipe -> ('l, 'a, 'c, 'r0, 'r2) pipe

   val runPipe: (Void.t, unit, Void.t, unit, 'r) pipe -> 'r
   val injectLeftovers: ('i, 'i, 'o, 'u, 'r) pipe -> ('l, 'i, 'o, 'u, 'r) pipe

   (* val mapOutput: ('o1 -> 'o2) -> ('l, 'i, 'o1, 'u, 'r) pipe -> ('l, 'i, 'o2, 'u, 'r) pipe *)

   val fromList: 'a list -> ('l, 'i, 'a, 'u, 'o, unit) pipe
end
(* vim: set ft=sml tw=0 ts=3 sw=3: *)
