
functor Dynamorphism(
  structure F: FUNCTOR
  structure Cofree: FREE_COMONAD where type 'a F.t = 'a F.t):
sig
  val dyna: ('a Cofree.t F.t -> 'a) -> ('b -> 'b F.t) -> 'b -> 'a
end =
struct
  structure H = Hylomorphism(structure F = F)
  fun dyna alg coalg = Cofree.extract o H.hylo (fn x => Cofree.:< (alg x, x)) coalg
end
