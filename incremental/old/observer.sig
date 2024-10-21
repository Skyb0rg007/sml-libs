
signature OBSERVER =
sig

type 'a t

val new: ('a -> unit) -> 'a t
val observe: 'a t * 'a -> unit
val equals: 'a t * 'b t -> bool

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
