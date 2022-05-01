
signature RECURSIVE =
sig
  structure F: FUNCTOR
  structure Free: FREE_MONAD where type 'a F.t = 'a F.t
  structure Cofree: FREE_COMONAD where type 'a F.t = 'a F.t

  type t

  val project: t -> t F.t
  val embed: t F.t -> t

  (* Catamorphisms *)
  val cata: ('a F.t -> 'a) -> t -> 'a
  val para: ((t * 'a) F.t -> 'a) -> t -> 'a
  val zygo: ('b -> 'b F.t) -> (('b * 'a) F.t -> 'a) -> t -> 'a
  val histo: ('a Cofree.t F.t -> 'a) -> t -> 'a

  (* Anamorphisms *)
  val ana: ('a -> 'a F.t) -> 'a -> t
  val apo: ('a -> ('a, t) Either.either F.t) -> 'a -> t
  val futu: ('a -> 'a Free.t F.t) -> 'a -> t

  (* Hylomorphisms *)
  val hylo: ('b F.t -> 'b) -> ('a -> 'a F.t) -> 'a -> 'b
  val chrono: ('b Cofree.t F.t -> 'b) -> ('a -> 'a Free.t F.t) -> 'a -> 'b
end

functor Recursive(F: FUNCTOR) : RECURSIVE =
struct
  infix 5 :<
  structure F = F
  structure Free = FreeMonadFn(F)
  structure Cofree = FreeComonadFn(F)
  val op :< = Cofree.:<

  datatype t = Fix of t F.t

  fun project (Fix t) = t
  val embed = Fix

  fun cata alg = alg o F.map (cata alg) o project
  fun para alg = alg o F.map (fn x => (x, para alg x)) o project
  fun zygo _ = raise Fail "NYI"
  fun histo alg =
    let
      fun distHisto fc = F.map Cofree.extract fc :< F.map (distHisto o Cofree.unwrap) fc
      fun c x = distHisto (F.map (Cofree.duplicate o Cofree.map alg o c) (project x))
    in
      alg o Cofree.extract o c
    end
  fun ana _ = raise Fail "NYI"
  fun apo _ = raise Fail "NYI"
  fun futu _ = raise Fail "NYI"
  fun hylo _ = raise Fail "NYI"
  fun chrono _ = raise Fail "NYI"
end
