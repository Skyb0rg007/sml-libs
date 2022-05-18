
functor Anamorphism(
  structure F: FUNCTOR
  type t
  val embed: t F.t -> t):
sig
  val ana: ('a -> 'a F.t) -> 'a -> t
end =
struct
  structure A = GAnamorphism(
    structure F = F
    structure M = Identity
    type t = t
    val embed = embed
    fun dist x = x)

  val ana = A.ana
end
