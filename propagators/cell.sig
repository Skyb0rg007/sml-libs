
signature CELL =
sig

type 'a t

datatype 'a change =
   Changed of 'a
 | Unchanged

val new: 'a -> ('a * 'a -> 'a change) -> 'a t
val write: 'a t * 'a -> unit
val content: 'a t -> 'a

val watch: 'a t -> ('a -> unit) -> unit
val watch2: 'a t -> 'b t -> ('a * 'b -> unit) -> unit
val watch3: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c -> unit) -> unit

val equals: 'a t * 'a t -> bool

end

(* vim: set tw=0 ts=3 sw=3: *)
