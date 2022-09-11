
signature EITHER =
sig

datatype ('a, 'b) either = INL of 'a | INR of 'b

val isLeft: ('a, 'b) either -> bool
val isRight: ('a, 'b) either -> bool
val asLeft: ('a, 'b) either -> 'a option
val asRight: ('a, 'b) either -> 'b option

val map: ('a -> 'c) * ('b -> 'd) -> ('a, 'b) either -> ('c, 'd) either
val app: ('a -> unit) -> ('b -> unit) -> ('a, 'b) either -> unit
val fold: ('a * 'c -> 'c) -> ('b * 'c -> 'c) -> 'c -> ('a, 'b) either -> 'c
val proj: ('a, 'a) either -> 'a
val partition: ('a, 'b) either list -> 'a list * 'b list
val mapLeft: ('a -> 'c) -> ('a, 'b) either -> ('c, 'b) either
val mapRight: ('b -> 'c) -> ('a, 'b) either -> ('a, 'c) either
val appLeft: ('a -> unit) -> ('a, 'b) either -> unit
val appRight: ('b -> unit) -> ('a, 'b) either -> unit

end

(* vim: set tw=0 ts=3 sw=3: *)
