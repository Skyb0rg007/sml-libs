
signature PROFUNCTOR =
   sig
      type ('a, 'b) t

      (* Laws:
       *   dimap id id ≡ id
       *   dimap (f ∘ g) (h ∘ i) ≡ dimap g h ∘ dimap f i
       *   lmap id ≡ id
       *   lmap (f ∘ g) ≡ lmap f ∘ lmap g
       *   rmap id ≡ id
       *   rmap (f ∘ g) ≡ rmap f ∘ rmap g
       *   dimap f g ≡ lmap f ∘ rmap g
       *)
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

signature COSTRONG =
   sig
      include PROFUNCTOR

      val unfirst: ('a * 'c, 'b * 'c) t -> ('a, 'b) t
      val unsecond: ('c * 'a, 'c * 'b) t -> ('a, 'b) t
   end

signature CLOSED =
   sig
      include PROFUNCTOR

      val curry: ('a * 'b, 'c) t -> ('a, 'b -> 'c) t
      val closed: ('a, 'b) t -> ('x -> 'a, 'x -> 'b) t
   end

signature SIEVE =
   sig
      include PROFUNCTOR

      structure F: FUNCTOR

      val sieve: ('a, 'b) t -> 'a -> 'b F.t
   end

signature REPRESENTABLE =
   sig
      include SIEVE

      val tabulate: ('a -> 'b F.t) -> ('a, 'b) t
   end

signature COSIEVE =
   sig
      include PROFUNCTOR

      structure F: FUNCTOR

      val cosieve: ('a, 'b) t -> 'a F.t -> 'b
   end

signature COREPRESENTABLE =
   sig
      include COSIEVE

      val cotabulate: ('a F.t -> 'b) -> ('a, 'b) t
   end

(*****************************************************************************)

(* CHOICE + STRONG *)
signature AFFINE =
   sig
      include STRONG

      val left: ('a, 'b) t -> (('a, 'c) Either.either, ('b, 'c) Either.either) t
      val right: ('a, 'b) t -> (('c, 'a) Either.either, ('c, 'b) Either.either) t
   end

(* CHOICE + CLOSED *)
signature COAFFINE =
   sig
      include CLOSED

      val left: ('a, 'b) t -> (('a, 'c) Either.either, ('b, 'c) Either.either) t
      val right: ('a, 'b) t -> (('c, 'a) Either.either, ('c, 'b) Either.either) t
   end

(* REPRESENTABLE + APPLY F *)
signature TRAVERSING1 =
   sig
      include PROFUNCTOR

      structure F: APPLY

      val sieve: ('a, 'b) t -> 'a -> 'b F.t
      val tabulate: ('a -> 'b F.t) -> ('a, 'b) t
   end

(* COREPRESENTABLE + COAPPLY F *)
signature COTRAVERSING1 =
   sig
      include PROFUNCTOR

      structure F: COAPPLY

      val cosieve: ('a, 'b) t -> 'a F.t -> 'b
      val cotabulate: ('a F.t -> 'b) -> ('a, 'b) t
   end

(* REPRESENTABLE + APPLICATIVE F *)
signature TRAVERSING =
   sig
      include PROFUNCTOR

      structure F: APPLICATIVE

      val sieve: ('a, 'b) t -> 'a -> 'b F.t
      val tabulate: ('a -> 'b F.t) -> ('a, 'b) t
   end

(* COREPRESENTABLE + COAPPLICATIVE F *)
signature COTRAVERSING =
   sig
      include PROFUNCTOR

      structure F: COAPPLICATIVE

      val cosieve: ('a, 'b) t -> 'a F.t -> 'b
      val cotabulate: ('a F.t -> 'b) -> ('a, 'b) t
   end

(*****************************************************************************)

(*
 * Equality
 * Iso          PROFUNCTOR
 * Prism        CHOICE
 * Coprism      COCHOICE
 * Lens         STRONG
 * Colens       COSTRONG
 * Grate        CLOSED
 * Traversal0   AFFINE (CHOICE + STRONG)
 * Cotraversal0 COAFFINE (CHOICE + CLOSED)
 * Traversal    AFFINE + TRAVERSING
 * Cotraversal  COAFFINE + COTRAVERSING
 * Traversal1   STRONG + TRAVERSING1
 * Cotraversal1 CLOSED + COTRAVERSING1
 * Mapping      REPRESENTABLE + DISTRIBUTIVE F
 *)

(*****************************************************************************)

structure Function =
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

structure Proxy =
   struct
      type 'a t = unit

      fun map _ () = ()
   end

structure Tagged =
   struct
      type ('a, 'b) t = 'b

      fun dimap _ g p = g p
      fun lmap _ p = p
      fun rmap g p = g p

      val left = Either.INL
      val right = Either.INR

      fun curry x _ = x
      fun closed x _ = x

      structure F = Proxy

      fun cosieve x () = x
      fun cotabulate f = f ()
   end

structure X: PROFUNCTOR = Tagged
structure X: CHOICE = Tagged
structure X: STRONG = Tagged
structure X: REPRESENTABLE = Tagged

functor Star(F: FUNCTOR) =
   struct
      type ('a, 'b) t = 'a -> 'b F.t

      structure F = F

      fun dimap f g p = F.map g o p o f
      fun lmap f p = p o f
      fun rmap g p = F.map g o p

      fun first p (a, c) = F.map (fn b => (b, c)) (p a)
      fun second p (c, a) = F.map (fn b => (c, b)) (p a)
      fun strong f p = dimap (fn a => (a, a)) f (second p)
      fun uncurry p = rmap Fn.apply (first p)

      fun sieve f = f
      fun tabulate f = f
   end

functor Const(type t):
   sig
      type 'a t = t

      val map: ('a -> 'b) -> 'a t -> 'b t
   end =
   struct
      type 'a t = t

      fun map _ x = x
   end

(* View:
 *    P := Star(Const(r))
 *    fun view p = p (fn r => r)
 *
 * Setter:
 *    P := Function
 *    fun over p f x = p f x
 *    fun set p y x = p (Fn.const y) x
 *
 * Review:
 *    P := Tagged
 *    fun review p = p (fn r => r)
 *)



functor Setter(
      type s
      type t
      type a
      type b
      val abst: (a -> b) -> s -> t
      structure P:
         sig
            include PROFUNCTOR

            structure F: DISTRIBUTIVE

            val sieve: ('a, 'b) t -> 'a -> 'b F.t
            val tabulate: ('a -> 'b F.t) -> ('a, 'b) t
         end
      val p: (a, b) P.t):
   sig
      val p: (s, t) P.t
   end=
   struct
      datatype ('a, 'b, 's) index = Index of 'a * ('b -> 's)

      val p: (s, t) P.t =
         P.dimap
            (fn a => Index (a, fn b => b))
            (fn Index (s, ab) => abst ab s)
            (P.tabulate (fn Index (a, bs) =>
               P.F.tabulate (fn x =>
                  Index (a, fn b => P.F.sub (P.sieve p (bs b)) x))))
   end

(* vim: set tw=0 ts=3 sw=3: *)
