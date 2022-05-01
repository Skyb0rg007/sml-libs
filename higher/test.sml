
signature MONAD =
sig
  type t
  val pure: 'a -> ('a, t) Higher.app
  val bind: ('a, t) Higher.app -> ('a -> ('b, t) Higher.app) -> ('b, t) Higher.app
end

signature COMONAD =
sig
  type t
  val extract: ('a, t) Higher.app -> 'a
  val extend: (('a, t) Higher.app -> 'b) -> ('a, t) Higher.app -> ('b, t) Higher.app
end

functor ComonadExtras(W: COMONAD) =
struct
  fun map f = W.extend (f o W.extract)
  fun duplicate w = W.extend Fn.id w
end

structure Prod:
sig
  type t
  val pair: 'a * 'b -> ('a, ('b, t) Higher.app) Higher.app
  val fst: ('a, ('b, t) Higher.app) Higher.app -> 'a
  val snd: ('a, ('b, t) Higher.app) Higher.app -> 'b

  val extract: ('a, ('b, t) Higher.app) Higher.app -> 'a
end =
struct
  structure T = Newtype2(type ('a, 'b) t = 'a * 'b)
  type t = T.t
  val pair = T.inj
  fun fst p = #1 (T.prj p)
  fun snd p = #2 (T.prj p)

  val extract = fst
  fun extend f p = T.inj (f p, snd p)
end

structure Test =
struct

end
