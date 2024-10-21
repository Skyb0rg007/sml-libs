
signature SF =
sig

type ('a, 'b) t

(* Category *)
val identity: ('a, 'a) t
val >>> : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
val <<< : ('b, 'c) t * ('a, 'b) t -> ('a, 'c) t

(* Arrow *)
val arr: ('a -> 'b) -> ('a, 'b) t
val first: ('a, 'b) t -> ('a * 'c, 'b * 'c) t
val second: ('a, 'b) t -> ('c * 'a, 'c * 'b) t
val *** : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
val &&& : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
val >>^ : ('a, 'b) t * ('b -> 'c) -> ('a, 'c) t
val ^>> : ('a -> 'b) * ('b, 'c) t -> ('a, 'c) t
val ^<< : ('b -> 'c) * ('a, 'b) t -> ('a, 'c) t
val <<^ : ('b, 'c) t * ('a -> 'b) -> ('a, 'c) t
val constant: 'a -> ('b, 'a) t
val swap: ('a * 'b, 'b * 'a) t
val dup: ('a, 'a * 'a) t

(* ArrowChoice *)
val left: ('a, 'b) t -> (('a, 'c) Either.either, ('b, 'c) Either.either) t
val right: ('a, 'b) t -> (('c, 'a) Either.either, ('c, 'b) Either.either) t
val +++ : ('a, 'b) t * ('c, 'd) t -> (('a, 'c) Either.either, ('b, 'd) Either.either) t
val ||| : ('a, 'c) t * ('b, 'c) t -> (('a, 'b) Either.either, 'c) t

(* Delays *)
(* Delay by one sample, using input for first output *)
val pre: 'a -> ('a, 'a) t
(* Replaces first output *)
val post: 'b -> ('a, 'b) t -> ('a, 'b) t
(* Prepends first output, shifting other outputs *)
val next: 'b -> ('a, 'b) t -> ('a, 'b) t
(* Buffers output *)
val fifo: ('a list, 'a option) t

(* State *)
val feedback: 'c -> ('a * 'c, 'b * 'c) t -> ('a, 'b) t
val scan: ('a * 'b -> 'b) -> 'b -> ('a, 'b) t
val mapAccum: ('a * 'c -> 'b * 'c) -> 'c -> ('a, 'b) t
val switch: ('a, (('a, 'b) t, 'b) Either.either) t -> ('a, 'b) t
val unfold: ('a -> 'b * 'a) -> 'a -> (unit, 'b) t

(* Time *)
val localTime: (unit, real) t
val integral: (real, real) t

(* Running a signal function *)
val reactimate:
   {initial: 'a,
    input: unit -> real * 'a option, (* Delta time and value *)
    output: 'b -> bool} (* Return true to stop the loop *)
   -> ('a, 'b) t
   -> unit

end

(* vim: set tw=0 ts=3 sw=3: *)
