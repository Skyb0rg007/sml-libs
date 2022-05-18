
functor Catamorphism(
  structure F: FUNCTOR
  type t
  val project: t -> t F.t):
sig
  val cata: ('a F.t -> 'a) -> t -> 'a
end =
struct
  structure C = GCatamorphism(
    structure F = F
    structure W = Identity
    type t = t
    val project = project
    fun dist x = x)

  val cata = C.cata
end
