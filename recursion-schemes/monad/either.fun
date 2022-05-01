
functor EitherMonad(type t) =
struct
  type 'a t = (t, 'a) Either.either

  fun map f = Either.map (Fn.id, f)
  val pure = Either.INR
  fun ap (Either.INR f, Either.INR x) = Either.INR (f x)
    | ap (Either.INL x, _) = Either.INL x
    | ap (_, Either.INL x) = Either.INL x
  fun bind (Either.INL x) _ = Either.INL x
    | bind (Either.INR y) f = f y
end
