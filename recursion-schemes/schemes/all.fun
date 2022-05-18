
functor SchemesAll(
  structure F: FUNCTOR
  type t
  val project: t -> t F.t
  val embed: t F.t -> t):
sig
  structure F: FUNCTOR
  structure Free: FREE_MONAD where type 'a F.t = 'a F.t
  structure Cofree: FREE_COMONAD where type 'a F.t = 'a F.t
  type t

  val cata: ('a F.t -> 'a) -> t -> 'a
  val histo: ('a Cofree.t F.t -> 'a) -> t -> 'a

  val ana: ('a -> 'a F.t) -> 'a -> t
  val futu: ('a -> 'a Free.t F.t) -> 'a -> t

  val hylo: ('b F.t -> 'b) -> ('a -> 'a F.t) -> 'a -> 'b
  val chrono: ('b Cofree.t F.t -> 'b) -> ('a -> 'a Free.t F.t) -> 'a -> 'b

  val para: ((t * 'a) F.t -> 'a) -> t -> 'a
  val apo: ('a -> (t, 'a) Either.either F.t) -> 'a -> t
  val elgot: ('b F.t -> 'b) -> ('a -> ('b, 'a F.t) Either.either) -> 'a -> 'b
  val coelgot: ('a * 'b F.t -> 'b) -> ('a -> 'a F.t) -> 'a -> 'b

  val mutu: (('a * 'b) F.t -> 'a) -> (('a * 'b) F.t -> 'b) -> (t -> 'a) * (t -> 'b)
  val zygo: (('a * 'b) F.t -> 'a) -> ('b F.t -> 'b) -> t -> 'a
  val dyna: ('a Cofree.t F.t -> 'a) -> ('b -> 'b F.t) -> 'b -> 'a
end =
struct
  structure X =
    struct
      structure F = F
      structure Free = FreeMonadFn(F)
      structure Cofree = FreeComonadFn(F)
      type t = t
      val project = project
      val embed = embed
    end
  open X

  structure Cata = Catamorphism(X)
  structure Histo = Histomorphism(X)
  structure Ana = Anamorphism(X)
  structure Futu = Futumorphism(X)
  structure Hylo = Hylomorphism(X)
  structure Chrono = Chronomorphism(X)
  structure Para = Paramorphism(X)
  structure Apo = Apomorphism(X)
  structure Elgot = Elgot(X)
  structure Coelgot = Coelgot(X)

  val cata = Cata.cata
  val histo = Histo.histo
  val ana = Ana.ana
  val futu = Futu.futu
  val hylo = Hylo.hylo
  val chrono = Chrono.chrono
  val para = Para.para
  val apo = Apo.apo
  val elgot = Elgot.elgot
  val coelgot = Coelgot.coelgot

  fun mutu alg1 alg2 =
    let
      fun alg x = (alg1 x, alg2 x)
    in
      (#1 o cata alg, #2 o cata alg)
    end

  fun zygo alg1 alg2 = #1 (mutu alg1 (alg2 o F.map #2))

  fun dyna alg coalg = Cofree.extract o hylo (fn x => Cofree.:< (alg x, x)) coalg
end
