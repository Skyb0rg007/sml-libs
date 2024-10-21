
signature BEHAVIOR =
sig

type ('a, 'b) t

val map: ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
val apply: ('a -> 'b, 'a -> 'c) t * ('a, 'c) t -> ('b, 'c) t
val pure: 'a -> ('a, 'b) t
val step: 'a -> 'a Event.t -> ('a, 'b) t
val sample: ('a, 'b) t -> ('a -> 'b) Event.t -> 'b Event.t
val sampleBy: ('a * 'b -> 'c) -> ('a, 'c) t -> 'b Event.t -> 'c Event.t
val sample_: ('a, 'a) t -> 'b Event.t -> 'a Event.t
val switcher: ('a, 'b) t -> ('a, 'b) t Event.t -> ('a, 'b) t
val fix: 'a -> (('a, 'b) t -> ('a, 'a) t) -> ('a, 'c) t

end

(* vim: set tw=0 ts=3 sw=3: *)
