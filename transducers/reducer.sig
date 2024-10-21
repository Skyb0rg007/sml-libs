
signature REDUCER =
sig
  type ('a, 'b) t

  (* Profunctor *)
  val lmap : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val rmap : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val dimap : ('a -> 'b) -> ('c -> 'd) -> ('b, 'c) t -> ('a, 'd) t

  (* Choice Profunctor *)
  val left : ('a, 'b) t -> (('a, 'c) Either.either, ('b, 'c) Either.either) t
  val right : ('a, 'b) t -> (('c, 'a) Either.either, ('c, 'b) Either.either) t

  (* Costrong Profunctor *)
  val unfirst : ('a * (unit -> 'c), 'b * (unit -> 'c)) t -> ('a, 'b) t
  val unsecond : ((unit -> 'c) * 'a, (unit -> 'c) * 'b) t -> ('a, 'b) t

  (* Corepresentable Profunctor *)
  structure F :
    sig
      type 'a t = 'a list
      val map : ('a -> 'b) -> 'a t -> 'b t
    end

  val cosieve : ('a, 'b) t -> 'a F.t -> 'b
  val cotabulate : ('a F.t -> 'b) -> ('a, 'b) t

  (* Applicative *)
  val pure : 'b -> ('a, 'b) t
  val map2 : ('b * 'c -> 'd) -> ('a, 'b) t * ('a, 'c) t -> ('a, 'd) t
  val both : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t

  (* Comonad *)
  val extract : ('a, 'b) t -> 'b
  val duplicate : ('a, 'b) t -> ('a, ('a, 'b) t) t
  val extend : (('a, 'b) t -> 'c) -> ('a, 'b) t -> ('a, 'c) t

  (* Arrow-like *)
  val &&& : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
  val *** : ('a, 'c) t * ('b, 'd) t -> ('a * 'b, 'c * 'd) t
  val either : ('a, 'c) t * ('b, 'd) t -> (('a, 'b) Either.either, 'c * 'd) t

  (* Folds *)
  val length : ('a, int) t
  val all : ('a -> bool) -> ('a, bool) t
  val exists : ('a -> bool) -> ('a, bool) t
  val fold : ('a * 'b -> 'b) -> 'b -> ('a, 'b) t

  (* Combinators *)
  val prefilter : ('a -> bool) -> ('a, 'b) t -> ('a, 'b) t
  val premapPartial : ('a -> 'b option) -> ('b, 'c) t -> ('a, 'c) t
  val compose : ('b, 'c) t * ('a, 'b) t -> ('a, 'c) t
  val take : int -> ('a, 'b) t -> ('a, 'b) t
  val drop : int -> ('a, 'b) t -> ('a, 'b) t
end
