
signature PROFUNCTOR =
   sig
      type ('a, 'b) t

      val dimap: ('a -> 'b) -> ('c -> 'd) -> ('b, 'c) t -> ('a, 'd) t
      val lmap: ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
      val rmap: ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
   end

signature CHOICE =
   sig
      include PROFUNCTOR

      val left: ('a, 'b) t -> (('a, 'c) Either.either, ('b, 'c) Either.either) t
      val right: ('a, 'b) t -> (('c, 'a) Either.either, ('c, 'b) Either.either) t
   end

signature STRONG =
   sig
      include PROFUNCTOR

      val first: ('a, 'b) t -> ('a * 'c, 'b * 'c) t
      val second: ('a, 'b) t -> ('c * 'a, 'c * 'b) t
      val uncurry: ('a, 'b -> 'c) t -> ('a * 'b, 'c) t
      val strong: ('a * 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
   end

signature CLOSED =
   sig
      include PROFUNCTOR

      val curry: ('a * 'b, 'c) t -> ('a, 'b -> 'c) t
      val closed: ('a, 'b) t -> ('x -> 'a, 'x -> 'b) t
   end

structure Arrow =
   struct
      type ('a, 'b) t = 'a -> 'b

      fun dimap f g p = g o p o f
      fun lmap f p = p o f
      fun rmap g p = g o p
      fun left p (Either.INL a) = Either.INL (p a)
        | left _ (Either.INR c) = Either.INR c
      fun right _ (Either.INL c) = Either.INL c
        | right p (Either.INR a) = Either.INR (p a)
      fun first p (x, y) = (p x, y)
      fun second p (x, y) = (x, p y)
      fun uncurry p (x, y) = p x y
      fun strong f p x = f (x, p x)
      fun closed p k x = p (k x)
      fun curry p x y = p (x, y)
   end

structure X: CHOICE = Arrow
structure X: STRONG = Arrow
structure X: CLOSED = Arrow

functor ListVectorIso(P: PROFUNCTOR) =
   struct
      fun iso p = P.dimap Vector.toList Vector.fromList p
   end

structure LV =
struct
   type alpha = int list
   type beta = bool list

   structure P =
      struct
         datatype ('a, 'b) t = T of ('a -> alpha) * (beta -> 'b)
         fun dimap f g (T (to, from)) = T (to o f, g o from)
         fun lmap f (T (to, from)) = T (to o f, from)
         fun rmap g (T (to, from)) = T (to, g o from)
      end

   structure I = ListVectorIso(P)

   fun iso (P.T (to, from)) = I.iso (P.T (from, to))

end

structure Iso =
   struct
      datatype ('s, 't, 'a, 'b) t = T of ('s -> 'a) * ('b -> 't)

      fun invert (T (a, b)) = T (b, a)

      fun dimap f g (T (a, b)) = T (f o a, b o g)
      fun lmap f (T (a, b)) = T (f o a, b)
      fun rmap g (T (a, b)) = T (a, b o g)
   end

structure Prism =
   struct
      datatype ('s, 't, 'a, 'b) t = T of ('s -> ('t, 'a) Either.either) * ('b -> 't)

      fun fromIso (Iso.T (a, b)) = T (Either.INR o a, b)

      fun dimap f g (T (a, b)) = T (Either.mapLeft g o a o f, g o b)
      fun lmap f (T (a, b)) = T (a o f, b)
      fun rmap g (T (a, b)) = T (Either.mapLeft g o a, g o b)
      fun left (T (a, b)) = T (fn Either.INL x => Either.mapLeft Either.INL (a x) | Either.INR y => Either.INL (Either.INR y), Either.INL o b)
      fun right (T (a, b)) = T (fn Either.INR x => Either.mapLeft Either.INR (a x) | Either.INL y => Either.INL (Either.INL y), Either.INR o b)
   end

structure Lens =
   struct
      datatype ('s, 't, 'a, 'b) t = T of ('s -> 'a) * ('s * 'b -> 't)

      fun fromIso (Iso.T (a, b)) = T (a, fn (_, x) => b x)

      fun dimap f g (T (a, b)) = T (a o f, fn (a, t) => g (b (f a, t)))
      fun lmap f (T (a, b)) = T (a o f, fn (a, t) => b (f a, t))
      fun rmap g (T (a, b)) = T (a, g o b)
      fun first (T (a, b)) = T (fn (x, _) => a x, fn ((x, y), z) => (b (x, z), y))
      fun second (T (a, b)) = T (fn (_, y) => a y, fn ((x, y), z) => (x, b (y, z)))
      fun uncurry p = rmap (fn (f, x) => f x) (first p)
      fun strong f p = dimap (fn x => (x, x)) (fn (x, y) => f (y, x)) (first p)
   end

structure View =
   struct
      datatype ('a, 'b) t = T of 'a -> 'b

      fun fromLens (Lens.T (a, _)) = T a

      fun view (T f, x) = f x
   end

structure Setter =
   struct
      datatype ('s, 't, 'a, 'b) t = T of ('a -> 'b) * 's -> 't

      fun fromLens (Lens.T (a, b)) = T (fn (f, s) => b (s, f (a s)))

      fun set (T f) x s = f (fn _ => x, s)
      fun over (T f) k s = f (k, s)
   end

structure Test =
struct
   val listToVector = Iso.T (Vector.toList, Vector.fromList)
end

(* vim: set tw=0 ts=3 sw=3: *)
