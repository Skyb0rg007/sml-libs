
functor Paramorphism(
  structure F: FUNCTOR
  type t
  val project: t -> t F.t
  val embed: t F.t -> t):
sig
  val para: ((t * 'a) F.t -> 'a) -> t -> 'a
end =
struct
  fun fst (x, _) = x
  fun snd (_, y) = y
  fun dist x = (embed (F.map fst x), F.map snd x)

  structure C = GCatamorphism(
    struct
      structure F = F
      structure W = EnvComonadFn(
        struct
          structure W = Identity
          type t = t
        end)
      type t = t
      val project = project
      val dist = dist
    end)

  val para = C.cata
end
