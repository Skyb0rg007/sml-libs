
functor ProxyFn(M: MONAD) :> PROXY where type 'a m = 'a M.t =
struct

infix  1 >>=
infixr 1 >=> =<< <=<

type 'a m = 'a M.t

datatype ('a2, 'a1, 'b2, 'b1, 'r) t =
   Request of 'a2 * ('a1 -> ('a2, 'a1, 'b2, 'b1, 'r) t)
 | Respond of 'b1 * ('b2 -> ('a2, 'a1, 'b2, 'b1, 'r) t)
 | M of ('a2, 'a1, 'b2, 'b1, 'r) t M.t
 | Pure of 'r

type 'r effect = (Void.t, unit, Void.t, unit, 'r) t
type ('b, 'r) producer = (Void.t, unit, unit, 'b, 'r) t
type ('a, 'b, 'r) pipe = (unit, 'a, unit, 'b, 'r) t
type ('a, 'r) consumer = (unit, 'a, unit, Void.t, 'r) t
type ('a2, 'a1, 'r) client = ('a2, 'a1, unit, Void.t, 'r) t
type ('b2, 'b1, 'r) server = (Void.t, unit, 'b2, 'b1, 'r) t

fun lift m = M (M.map Pure m)

fun reflect (Request (a', fa)) = Respond (a', fn a => reflect (fa a))
  | reflect (Respond (b, fb')) = Request (b, fn b' => reflect (fb' b'))
  | reflect (M m)              = M (M.map reflect m)
  | reflect (Pure r)           = Pure r

fun run (Request (v, _)) = Void.absurd v
  | run (Respond (v, _)) = Void.absurd v
  | run (M m)            = M.>>= (m, run)
  | run (Pure r)         = M.pure r

structure Kleisli =
   struct
      val pure = Pure

      fun map f (Request (v, k)) = Request (v, map f o k)
        | map f (Respond (v, k)) = Respond (v, map f o k)
        | map f (M m)            = M (M.map (map f) m)
        | map f (Pure r)         = Pure (f r)

      fun (Request (a', fa)) >>= f = Request (a', fn a => fa a >>= f)
        | (Respond (b, fb')) >>= f = Respond (b, fn b' => fb' b' >>= f)
        | (M m)              >>= f = M (M.map (fn x => x >>= f) m)
        | (Pure r)           >>= f = f r

      fun (f >=> g) x = f x >>= g

      fun f =<< p = p >>= f
      fun g <=< f = f >=> g
   end

structure Respond =
   struct
      fun pure a = Respond (a, Pure)

      fun (Request (x', fx)) >>= fb = Request (x', fn x => fx x >>= fb)
        | (Respond (b, fb')) >>= fb = Kleisli.>>= (fb b, fn b' => fb' b' >>= fb)
        | (M m)              >>= fb = M (M.map (fn x => x >>= fb) m)
        | (Pure a)           >>= _  = Pure a

      fun (f >=> g) x = f x >>= g
   end

structure Request =
   struct
      fun pure a = Request (a, Pure)

      fun fb' =<< (Request (b', fb)) = Kleisli.>>= (fb' b', fn b => fb' =<< fb b)
        | fb' =<< (Respond (x, fx')) = Respond (x, fn x' => fb' =<< fx' x')
        | fb' =<< (M m)              = M (M.map (fn x => fb' =<< x) m)
        | _   =<< (Pure a)           = Pure a

      fun (f <=< g) x = f =<< g x
   end

fun pullBind (fb', Request (b', fb)) = pushBind (fb' b', fb)
  | pullBind (fb', Respond (c, fc')) = Respond (c, fn c' => pullBind (fb', fc' c'))
  | pullBind (fb', M m)  = M (M.map (fn x => pullBind (fb', x)) m)
  | pullBind (_, Pure r) = Pure r

and pushBind (Request (a', fa), fb) = Request (a', fn a => pushBind (fa a, fb))
  | pushBind (Respond (b, fb'), fb) = pullBind (fb', fb b)
  | pushBind (M m, fb)   = M (M.map (fn x => pushBind (x, fb)) m)
  | pushBind (Pure r, _) = Pure r

structure Pull =
   struct
      fun pure a' = Request (a', fn a => Respond (a, pure))

      val op =<< = pullBind

      fun (f <=< g) x = f =<< g x
   end

structure Push =
   struct
      fun pure a = Respond (a, fn a' => Request (a', pure))

      val op >>= = pushBind

      fun (f >=> g) x = f x >>= g
   end

structure Prelude =
   struct
      fun take n =
         if n <= 0
            then Kleisli.pure ()
         else Kleisli.>>= (Request.pure (), fn a =>
              Kleisli.>>= (Respond.pure a, fn () =>
              take (n - 1)))

      fun drop n =
         if n <= 0
            then Pull.pure ()
         else Kleisli.>>= (Request.pure (), fn _ => drop (n - 1))

      fun takeWhile p =
         Kleisli.>>= (Request.pure (), fn a =>
         if p a
            then Kleisli.>>= (Respond.pure a, fn () => takeWhile p)
         else Kleisli.pure ())

      fun takeWhile' p =
         Kleisli.>>= (Request.pure (), fn a =>
         if p a
            then Kleisli.>>= (Respond.pure a, fn () => takeWhile' p)
         else Kleisli.pure a)

      fun dropWhile p =
         Kleisli.>>= (Request.pure (), fn a =>
         if p a
            then dropWhile p
         else Kleisli.>>= (Respond.pure a, fn () => Pull.pure ()))

      fun replicate n m = Request.=<< (fn _ => lift m, take n)

      fun fromList xs =
         List.foldr (fn (a, p) => Kleisli.>>= (Respond.pure a, fn () => p)) (Kleisli.pure ()) xs

      fun findIndices p =
         let
            fun go n =
               Kleisli.>>= (Request.pure (), fn a =>
               if p a
                  then Kleisli.>>= (Respond.pure n, fn _ => go (n + 1))
               else go (n + 1))
         in
            go 0
         end

      fun next (Request (v, _))  = Void.absurd v
        | next (Respond (a, fu)) = M.pure (Either.INR (a, fu ()))
        | next (M m)             = M.>>= (m, next)
        | next (Pure r)          = M.pure (Either.INL r)
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
