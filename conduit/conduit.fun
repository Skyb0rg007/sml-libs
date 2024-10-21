
functor ConduitFn'(M: MONAD) =
struct

nonfix o
infixr 1 >=>
infix 1 >>=

datatype ('l, 'i, 'o, 'u, 'r) pipe =
   HaveOutput of ('l, 'i, 'o, 'u, 'r) pipe * 'o
 | NeedInput of ('i -> ('l, 'i, 'o, 'u, 'r) pipe) * ('u -> ('l, 'i, 'o, 'u, 'r) pipe)
 | Done of 'r
 | PipeM of ('l, 'i, 'o, 'u, 'r) pipe M.t
 | Leftover of ('l, 'i, 'o, 'u, 'r) pipe * 'l

type 'a m = 'a M.t
type ('i, 'o, 'r) t = ('i, 'i, 'o, unit, 'r) pipe

fun runPipe (HaveOutput (_, o)) = Void.absurd o
  | runPipe (NeedInput (_, c)) = runPipe (c ())
  | runPipe (Done r) = M.pure r
  | runPipe (PipeM mp) = M.>>= (mp, runPipe)
  | runPipe (Leftover (_, i)) = Void.absurd i

fun injectLeftovers p =
   let
      fun go (ls, HaveOutput (p, o)) = HaveOutput (go (ls, p), o)
        | go (l :: ls, NeedInput (p, _)) = go (ls, p l)
        | go ([], NeedInput (p, c)) = NeedInput (fn x => go ([], p x), fn x => go ([], c x))
        | go (_, Done r) = Done r
        | go (ls, PipeM mp) = PipeM (M.map (fn x => go (ls, x)) mp)
        | go (ls, Leftover (p, l)) = go (l :: ls, p)
   in
      go ([], p)
   end

fun run p = runPipe (injectLeftovers p)

val pure = Done

fun pipe >>= fp =
   case pipe of
      HaveOutput (p, o) => HaveOutput (p >>= fp, o)
    | NeedInput (p, c) => NeedInput (fn x => p x >>= fp, fn x => c x >>= fp)
    | Done x => fp x
    | PipeM m => PipeM (M.map (fn x => x >>= fp) m)
    | Leftover (p, i) => Leftover (p >>= fp, i)

fun (f >=> g) x = f x >>= g

fun map f p = p >>= (fn x => pure (f x))

fun lift m = PipeM (M.map Done m)

val await = NeedInput (fn i => Done (SOME i), fn _ => Done NONE)

val awaitE = NeedInput (fn i => Done (Either.INR i), fn u => Done (Either.INL u))

fun awaitForever inner =
   awaitE >>=
      (fn Either.INL u => pure u
        | Either.INR i => inner i >>= (fn _ => awaitForever inner))

fun yield o = HaveOutput (Done (), o)

fun yieldM m = PipeM (M.map (fn o => HaveOutput (Done (), o)) m)

fun leftover l = Leftover (Done (), l)

fun unconsM (HaveOutput (p, o)) = M.pure (SOME (o, p))
  | unconsM (NeedInput (_, c)) = unconsM (c ())
  | unconsM (Done ()) = M.pure NONE
  | unconsM (PipeM mp) = M.>>= (mp, unconsM)
  | unconsM (Leftover (_, i)) = Void.absurd i

local
   fun id' () = NeedInput (fn x => HaveOutput (id' (), x), Done)
in
   val id = NeedInput (fn x => HaveOutput (id' (), x), Done)
end

local
   fun goRight (l, r) =
      case r of
         HaveOutput (p, o) => HaveOutput (goRight (l, p), o)
       | NeedInput (rp, rc) => goLeft (rp, rc, l)
       | Done r2 => Done r2
       | PipeM mp => PipeM (M.map (fn p => goRight (l, p)) mp)
       | Leftover (_, i) => Void.absurd i

   and goLeft (rp, rc, l) =
      case l of
         HaveOutput (l', o) => goRight (l', rp o)
       | NeedInput (l', lc) => NeedInput (fn x => goLeft (rp, rc, l' x), fn x => goLeft (rp, rc, lc x))
       | Done r1 => goRight (Done r1, rc r1)
       | PipeM mp => PipeM (M.map (fn x => goLeft (rp, rc, x)) mp)
       | Leftover (l', i) => Leftover (goLeft (rp, rc, l'), i)
in
   val pipe = goRight
end

local
   fun goRight (l, r) =
      case r of
         HaveOutput (p, o) => HaveOutput (goRight (l, p), o)
       | NeedInput (rp, rc) => goLeft (rp, rc, l)
       | Done r2 => Done r2
       | PipeM mp => PipeM (M.map (fn p => goRight (l, p)) mp)
       | Leftover (r', i) => goRight (HaveOutput (l, i), r')

   and goLeft (rp, rc, l) =
      case l of
         HaveOutput (l', o) => goRight (l', rp o)
       | NeedInput (l', lc) => NeedInput (fn x => goLeft (rp, rc, l' x), fn x => goLeft (rp, rc, lc x))
       | Done r1 => goRight (Done r1, rc r1)
       | PipeM mp => PipeM (M.map (fn x => goLeft (rp, rc, x)) mp)
       | Leftover (l', i) => Leftover (goLeft (rp, rc, l'), i)
in
   val pipeL = goRight
end

val |> = pipeL

end

functor ConduitFn(M: MONAD): CONDUIT = ConduitFn'(M)

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
