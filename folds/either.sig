
signature EITHER =
sig

datatype ('a,'b) either = INL of 'a | INR of 'b

val isLeft: ('a,'b) either -> bool
val isRight: ('a,'b) either -> bool
val asLeft: ('b,'a) either -> 'b option
val asRight: ('a,'b) either -> 'b option
val map: ('a -> 'c) * ('b -> 'd) -> ('a,'b) either -> ('c,'d) either
val mapLeft: ('a -> 'b) -> ('a,'c) either -> ('b,'c) either
val mapRight: ('a -> 'c) -> ('b,'a) either -> ('b,'c) either
val app: ('a -> unit) * ('b -> unit) -> ('a,'b) either -> unit
val appLeft: ('a -> unit) -> ('a,'b) either -> unit
val appRight: ('b -> unit) -> ('a,'b) either -> unit
val fold: ('a * 'c -> 'c) * ('b * 'c -> 'c) -> 'c -> ('a,'b) either -> 'c
val proj: ('a,'a) either -> 'a
val partition: ('a,'b) either list -> 'a list * 'b list

end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
