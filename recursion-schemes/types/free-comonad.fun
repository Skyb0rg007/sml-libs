
functor FreeComonadFn(F: FUNCTOR) :> FREE_COMONAD where type 'a F.t = 'a F.t =
struct
  infix 5 :<
  structure F = F

  datatype 'a t = :< of 'a * 'a t F.t

  fun map f (x :< y) = (f x :< F.map (map f) y)

  fun unwrap (_ :< y) = y

  fun extract (x :< _) = x

  fun extend f (x :< y) = f (x :< y) :< F.map (extend f) y

  fun duplicate (x :< y) = (x :< y) :< F.map duplicate y

  fun dist x = F.map extract x :< F.map (dist o unwrap) x
end
