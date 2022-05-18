
functor Coelgot(structure F: FUNCTOR):
sig
  val coelgot: ('a * 'b F.t -> 'b) -> ('a -> 'a F.t) -> 'a -> 'b
end =
struct
  fun coelgot alg coalg =
    let
      fun go x = alg (x, F.map go (coalg x))
    in
      go
    end
end
