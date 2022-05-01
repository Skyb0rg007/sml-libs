
functor FreeMonadFn(F: FUNCTOR) :> FREE_MONAD where type 'a F.t = 'a F.t =
struct
  structure F = F

  datatype 'a t =
     Pure of 'a 
   | Impure of 'a t F.t

  fun map f (Pure a) = Pure (f a)
    | map f (Impure fa) = Impure (F.map (map f) fa)

  val pure = Pure

  fun ap (Pure ab, Pure a) = Pure (ab a)
    | ap (Pure ab, Impure fa) = Impure (F.map (map ab) fa)
    | ap (Impure fab, ta) = Impure (F.map (fn ab => ap (ab, ta)) fab)

  fun bind (Pure a) f = f a
    | bind (Impure fa) f = Impure (F.map (fn a => bind a f) fa)

  fun join (Pure a) = a
    | join (Impure fa) = Impure (F.map join fa)
end
