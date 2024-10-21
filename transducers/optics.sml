
functor Optics(P : PROFUNCTOR) :
  sig
    type ('s, 't, 'a, 'b) t = ('a, 'b) P.t -> ('s, 't) P.t

    val id : ('a, 'b, 'a, 'b) t
    val iso : ('s -> 'a) -> ('b -> 't) -> ('s, 't, 'a, 'b) t
  end =
  struct
    type ('s, 't, 'a, 'b) t = ('a, 'b) P.t -> ('s, 't) P.t

    fun id x = x

    fun iso from to p = P.lmap from (P.rmap to p)
  end
