
signature LATER =
sig

type 'a t

val pure: 'a -> 'a t
val map: ('a -> 'b) -> 'a t -> 'b t
val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
val ap: ('a -> 'b) t * 'a t -> 'b t
val ap2: ('a * 'b -> 'c) t * 'a t * 'b t -> 'c t
val fix: ('a t -> 'a) -> 'a

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
