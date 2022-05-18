
functor Futumorphism(
  structure F: FUNCTOR
  structure Free: FREE_MONAD where type 'a F.t = 'a F.t
  type t
  val embed: t F.t -> t):
sig
  val futu: ('a -> 'a Free.t F.t) -> 'a -> t
end =
struct
  structure A = GAnamorphism(
    structure F = F
    structure M = Free
    type t = t
    val embed = embed
    val dist = Free.dist)

  val futu = A.ana
end
