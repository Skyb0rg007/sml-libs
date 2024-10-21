(* Mathematical structures' signatures along with their laws
 * Convensions:
 *   Use + for commutative operations and * otherwise
 *   (+, zero, ~) for operator, identity, inverse
 *   ( *, one, inv) for ""
 *)

(** Group-like (one binary operation) **)

signature MAGMA =
   sig
      type t

      val + : t * t -> t

      (* No laws *)
   end

signature SEMIGROUP =
   sig
      type t

      val * : t * t -> t

      (* Laws:
       *   assocativity
       *     ∀abc, (a * b) * c = a * (b * c)
       *)
   end

signature MONOID =
   sig
      type t

      val one: t
      val * : t * t -> t

      (* Laws:
       *   assocativity
       *     ∀abc, (a * b) * c = a * (b * c)
       *   identity
       *     ∀a, a * one = one * a = a
       *)
   end

signature COMMUTATIVE_MONOID =
   sig
      type t

      val zero: t
      val + : t * t -> t

      (* Laws:
       *   assocativity
       *     ∀abc, (a + b) + c = a + (b + c)
       *   commutativity
       *     ∀ab, a + b = b + a
       *   identity
       *     ∀a, a + zero = zero + a = a
       *)
   end

signature QUASIGROUP =
   sig
      type t

      val * : t * t -> t
      val \ : t * t -> t (* left division *)
      val / : t * t -> t (* right division *)

      (* Laws:
       *   Latin square property
       *     ∀ab, a * (a \ b) = b
       *     ∀ab, a \ (a * b) = b
       *     ∀ab, (b / a) * a = b
       *     ∀ab, (b * a) / a = b
       *)
   end

signature LOOP =
   sig
      type t

      val one: t
      val * : t * t -> t
      val \ : t * t -> t (* left division *)
      val / : t * t -> t (* right division *)

      (* Laws:
       *   Latin square property
       *     ∀ab, a * (a \ b) = b
       *     ∀ab, a \ (a * b) = b
       *     ∀ab, (b / a) * a = b
       *     ∀ab, (b * a) / a = b
       *   identity
       *     ∀a, a * one = one * a = a
       *)
   end

signature GROUP =
   sig
      type t

      val one: t
      val * : t * t -> t
      val inv: t -> t

      (* Laws:
       *   assocativity
       *     ∀abc, (a * b) * c = a * (b * c)
       *   identity
       *     ∀a, a * one = one * a = a
       *   inverse element
       *     ∀a, a * inv a = zero
       *)
   end

signature ABELIAN_GROUP =
   sig
      type t

      val zero: t
      val + : t * t -> t
      val ~ : t -> t

      (* Laws:
       *   assocativity
       *     ∀abc, (a + b) + c = a + (b + c)
       *   commutativity
       *     ∀ab, a + b = b + a
       *   identity
       *     ∀a, a + zero = zero + a = a
       *   inverse element
       *     ∀a, a + ~a = zero
       *)
   end

(****************************************************************************)
(** Ring-like (two binary operations) **)

signature RNG =
   sig
      type t

      val zero: t
      val + : t * t -> t
      val * : t * t -> t

      (* Laws:
       *   + is assocative
       *     ∀abc, (a + b) + c = a + (b + c)
       *   + is commutative
       *     ∀ab, a + b = b + a
       *   zero is the additive identity
       *     ∀a, a + zero = zero + a = a
       *   * is associative
       *     ∀abc, (a * b) * c = a * (b * c)
       *   multiplication distributes over addition
       *     ∀abc, a * (b + c) = (a * b) + (a * c)
       *     ∀abc, (a + b) * c = (a * c) + (b * c)
       *)
   end

signature SEMIRING =
   sig
      type t

      val zero: t
      val one: t
      val + : t * t -> t
      val * : t * t -> t

      (* Laws:
       *   + is assocative
       *     ∀abc, (a + b) + c = a + (b + c)
       *   + is commutative
       *     ∀ab, a + b = b + a
       *   zero is the additive identity
       *     ∀a, a + zero = zero + a = a
       *   * is associative
       *     ∀abc, (a * b) * c = a * (b * c)
       *   one is the multiplicative identity
       *     ∀ab, a * one = one * a = a
       *   multiplication distributes over addition
       *     ∀abc, a * (b + c) = (a * b) + (a * c)
       *     ∀abc, (a + b) * c = (a * c) + (b * c)
       *   multiplication by zero is an annihilator
       *     ∀a, zero * a = a * zero = zero
       *)
   end

signature STAR_SEMIRING =
   sig
      type t

      val zero: t
      val one: t
      val + : t * t -> t
      val * : t * t -> t
      val closure: t -> t

      (* Laws:
       *   + is assocative
       *     ∀abc, (a + b) + c = a + (b + c)
       *   + is commutative
       *     ∀ab, a + b = b + a
       *   zero is the additive identity
       *     ∀a, a + zero = zero + a = a
       *   * is associative
       *     ∀abc, (a * b) * c = a * (b * c)
       *   one is the multiplicative identity
       *     ∀ab, a * one = one * a = a
       *   multiplication distributes over addition
       *     ∀abc, a * (b + c) = (a * b) + (a * c)
       *     ∀abc, (a + b) * c = (a * c) + (b * c)
       *   multiplication by zero is an annihilator
       *     ∀a, zero * a = a * zero = zero
       *   star property
       *     ∀a, closure a = one + (a * closure a) = one + (closure a * a)
       *)
   end

signature KLEENE_ALGEBRA =
   sig
      type t

      val zero: t
      val one: t
      val + : t * t -> t
      val * : t * t -> t
      val closure: t -> t

      (* Laws:
       *   + is assocative
       *     ∀abc, (a + b) + c = a + (b + c)
       *   + is commutative
       *     ∀ab, a + b = b + a
       *   + is idempotent
       *     ∀a, a + a = a
       *   zero is the additive identity
       *     ∀a, a + zero = zero + a = a
       *   * is associative
       *     ∀abc, (a * b) * c = a * (b * c)
       *   one is the multiplicative identity
       *     ∀ab, a * one = one * a = a
       *   multiplication distributes over addition
       *     ∀abc, a * (b + c) = (a * b) + (a * c)
       *     ∀abc, (a + b) * c = (a * c) + (b * c)
       *   multiplication by zero is an annihilator
       *     ∀a, zero * a = a * zero = zero
       *   star property
       *     ∀a, closure a = one + (a * closure a) = one + (closure a * a)
       *)
   end

signature RING =
   sig
      type t

      val zero: t
      val one: t
      val + : t * t -> t
      val ~ : t -> t
      val * : t * t -> t

      (* Laws:
       *   + is assocative
       *     ∀abc, (a + b) + c = a + (b + c)
       *   + is commutative
       *     ∀ab, a + b = b + a
       *   ~a is the additive inverse of a
       *     ∀a, a + ~a = zero
       *   zero is the additive identity
       *     ∀a, a + zero = zero + a = a
       *   * is associative
       *     ∀abc, (a * b) * c = a * (b * c)
       *   one is the multiplicative identity
       *     ∀ab, a * one = one * a = a
       *   multiplication distributes over addition
       *     ∀abc, a * (b + c) = (a * b) + (a * c)
       *     ∀abc, (a + b) * c = (a * c) + (b * c)
       *)
   end

(* vim: set tw=0 ts=3 sw=3: *)
