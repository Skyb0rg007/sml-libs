
signature STREAM =
sig

type 'a t

val unfold: ('s -> ('a * 's) option) -> 's -> 'a t
val fold: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
val take: int -> 'a t -> 'a t
val map: ('a -> 'b) -> 'a t -> 'b t
val filter: ('a -> bool) -> 'a t -> 'a t
val concatMap: ('a -> 'b t) -> 'a t -> 'b t

end

(* vim: set tw=0 ts=3 sw=3: *)
