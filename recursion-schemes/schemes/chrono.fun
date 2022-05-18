
functor Chronomorphism(
  structure F: FUNCTOR
  structure Free: FREE_MONAD where type 'a F.t = 'a F.t
  structure Cofree: FREE_COMONAD where type 'a F.t = 'a F.t):
sig
  val chrono: ('b Cofree.t F.t -> 'b) -> ('a -> 'a Free.t F.t) -> 'a -> 'b
end =
struct
  structure H = GHylomorphism(
    struct
      structure F = F
      structure M = Free
      structure W = Cofree
      val distM = Free.dist
      val distW = Cofree.dist
    end)

  val chrono = H.hylo
end
