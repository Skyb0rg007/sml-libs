
signature UNFOLD =
sig

type ('a, 'b) t

val premap : ('a -> 'c) -> ('c, 'b) t -> ('a, 'b) t
val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

val takeWhileWithInput : ('a * 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t
val takeWhile : ('b -> bool) -> ('a, 'b) t -> ('a, 'b) t

val unfoldr : ('a -> ('b * 'a) option) -> ('a, 'b) t
val zipWith : ('b * 'c -> 'd) -> ('a, 'b) t * ('a, 'c) t -> ('a, 'd) t
val concatMap : ('b -> ('a, 'c) t) -> ('a, 'b) t -> ('a, 'c) t
val fromFunction : ('a -> 'b) -> ('a, 'b) t

val manyInterleave : ('a, 'b) t * ('c, 'a) t -> ('c, 'b) t

end
