
functor FreeMonadFn(F: FUNCTOR) :> FREE_MONAD where type 'a F.t = 'a F.t =
struct
  structure F = F

  datatype 'a t =
     Pure of 'a 
   | Impure of 'a t F.t

  fun wrap x = Impure x

  fun map f (Pure a) = Pure (f a)
    | map f (Impure fa) = Impure (F.map (map f) fa)

  val pure = Pure

  fun join (Pure a) = a
    | join (Impure fa) = Impure (F.map join fa)

  fun dist (Pure a) = F.map Pure a
    | dist (Impure a) = F.map (Impure o dist) a
end
