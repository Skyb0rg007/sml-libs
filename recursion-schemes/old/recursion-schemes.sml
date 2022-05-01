
functor CataFn(
  structure F: FUNCTOR
  structure Fix: FIX where type 'a F.t = 'a F.t) =
struct
  fun cata alg (Fix.Fix x) = alg (F.map (cata alg) x)
end

functor ParaFn(
  structure F: FUNCTOR
  structure Fix: FIX where type 'a F.t = 'a F.t) =
struct
  fun para alg (Fix.Fix x) = alg (F.map (fn a => (a, para alg a)) x)
end

(* functor GParaFn( *)
(*   structure F: FUNCTOR *)
(*   structure Fix: FIX where type 'a F.t = 'a F.t *)
(*   structure W: COMONAD *)
(*   val dist: 'a W.t F.t -> 'a F.t W.t *)
(* ) *)
