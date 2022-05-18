
functor Histomorphism(
  structure F: FUNCTOR
  structure Cofree: FREE_COMONAD where type 'a F.t = 'a F.t
  type t
  val project: t -> t F.t):
sig
  val histo: ('a Cofree.t F.t -> 'a) -> t -> 'a
end =
struct
  structure C = GCatamorphism(
    struct
      structure F = F
      structure W = Cofree
      type t = t
      val project = project
      val dist = Cofree.dist
    end)

  val histo = C.cata
end
