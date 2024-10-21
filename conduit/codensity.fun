
functor CodensityFn(T: sig type 'a t end) =
struct

infix 1 >>=

structure Cont = SMLofNJ.Cont
structure U = Universal

type 'a t = ('a -> U.universal T.t) -> U.universal T.t

fun map f m k = m (fn x => k (f x))
fun pure x k = k x
fun (m >>= k) c = m (fn a => k a c)
fun liftWith op >>= m k = m >>= k

fun lowerWith map pure m =
  let
    val tag = U.tag ()
  in
    map (U.tagProject tag) (m (pure o U.tagInject tag))
  end

val _ : ('a -> 'b) -> 'a t -> 'b t = map
val _ : 'a -> 'a t = pure
val _ : 'a t * ('a -> 'b t) -> 'b t = op >>=
val _ : ('a T.t * ('a -> U.universal T.t) -> U.universal T.t) -> 'a T.t -> 'a t = liftWith
val _ : ((U.universal -> 'a) -> U.universal T.t -> 'a T.t)
     -> (U.universal -> U.universal T.t)
     -> 'a t
     -> 'a T.t = lowerWith

end
