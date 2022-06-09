
structure Conduit : CONDUIT =
struct
   infix 1 >>=
   infixr 1 >=> <=< =<<

   datatype ('l, 'i, 'o, 'u, 'r) pipe =
      HaveOutput of ('l, 'i, 'o, 'u, 'r) pipe * 'o
    | NeedInput of ('i -> ('l, 'i, 'o, 'u, 'r) pipe) * ('u -> ('l, 'i, 'o, 'u, 'r) pipe)
    | Done of 'r
    | Leftover of ('l, 'i, 'o, 'u, 'r) pipe * 'l

   type ('i, 'o, 'r) t = ('i, 'i, 'o, unit, 'r) pipe

   val pure = Done

   fun map f (HaveOutput (p, out)) = HaveOutput (map f p, out)
     | map f (NeedInput (p, c)) = NeedInput (map f o p, map f o c)
     | map f (Done x) = Done (f x)
     | map f (Leftover (p, i)) = Leftover (map f p, i)

   fun (HaveOutput (p, out)) >>= f = HaveOutput (p >>= f, out)
     | (NeedInput (p, c)) >>= f = NeedInput (fn x => p x >>= f, fn x => c x >>= f)
     | (Done x: ('l, 'i, 'o, 'u, 'r) pipe) >>= f = f x
     | (Leftover (p, i)) >>= f = Leftover (p >>= f, i)

   fun (f >=> g) x = f x >>= g

   fun (f <=< g) x = g x >>= f

   fun f =<< p = p >>= f

   fun ap (f, x) = f >>= (fn f => x >>= (fn x => pure (f x)))

   val await = NeedInput (fn i => Done (SOME i), fn _ => Done NONE)

   val awaitE = NeedInput (fn i => Done (Either.INR i), fn u => Done (Either.INL u))

   fun awaitForever inner =
      let
         fun self () = awaitE >>= go
         and go (Either.INL u) = pure u
           | go (Either.INR i) = inner i >>= (fn _ => self ())
      in
         self ()
      end

   fun yield out = HaveOutput (Done (), out)

   fun leftover l = Leftover (Done (), l)

   fun uncons (HaveOutput (p, out)) = SOME (out, p)
     | uncons (NeedInput (_, c)) = uncons (c ())
     | uncons (Done ()) = NONE
     | uncons (Leftover (_, i)) = Void.absurd i

   fun unconsE (HaveOutput (p, out)) = Either.INR (out, p)
     | unconsE (NeedInput (_, c)) = unconsE (c ())
     | unconsE (Done r) = Either.INL r
     | unconsE (Leftover (_, i)) = Void.absurd i

   fun id () = NeedInput (fn out => HaveOutput (id (), out), Done)

   fun pipe (p1, p2) =
      let
         fun goRight (l, HaveOutput (p, out)) = HaveOutput (goRight (l, p), out)
           | goRight (l, NeedInput (rp, rc)) = goLeft (rp, rc, l)
           | goRight (l, Done r2) = Done r2
           | goRight (l, Leftover (_, i)) = Void.absurd i

         and goLeft (rp, rc, HaveOutput (l, out)) = goRight (l, rp out)
           | goLeft (rp, rc, NeedInput (l, lc)) = NeedInput (fn i => goLeft (rp, rc, l i), fn u => goLeft (rp, rc, lc u))
           | goLeft (rp, rc, Done r1) = goRight (Done r1, rc r1)
           | goLeft (rp, rc, Leftover (l, i)) = Leftover (goLeft (rp, rc, l), i)
      in
         goRight (p1, p2)
      end

   fun runPipe (HaveOutput (_, out)) = Void.absurd out
     | runPipe (NeedInput (_, c)) = runPipe (c ())
     | runPipe (Done r) = r
     | runPipe (Leftover (_, i)) = Void.absurd i

   fun injectLeftovers p =
      let
         fun go (ls, HaveOutput (p, out)) = HaveOutput (go (ls, p), out)
           | go (l::ls, NeedInput (p, _)) = go (ls, p l)
           | go ([], NeedInput (p, c)) = NeedInput (fn i => go ([], p i), fn u => go ([], c u))
           | go (_, Done r) = Done r
           | go (ls, Leftover (p, l)) = go (l::ls, p)
      in
         go ([], p)
      end

   fun fromList [] = Done ()
     | fromList (x::xs) = HaveOutput (fromList xs, x)
end
(* vim: set ft=sml tw=0 ts=3 sw=3: *)
