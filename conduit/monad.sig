
signature MONAD =
sig

type 'a t

val pure: 'a -> 'a t
val map: ('a -> 'b) -> 'a t -> 'b t
val >>= : 'a t * ('a -> 'b t) -> 'b t

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
