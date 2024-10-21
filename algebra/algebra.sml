
signature MAGMA =
   sig
      type t

      val * : t * t -> t
   end

signature UNITAL_MAGMA =
   sig
      include MAGMA

      (* Laws:
       *   one * x = x * one = x
       *)
      val one: t
   end

signature RIGHT_QUASIGROUP =
   sig
      include MAGMA

      (* Laws:
       *   (y / x) * x = y
       *   (y * x) / x = y
       *)
      val / : t * t -> t
   end

signature LEFT_QUASIGROUP =
   sig
      include MAGMA

      (* Laws:
       *   x * (x \ y) = y
       *   x \ (x * y) = y
       *)
      val \ : t * t -> t
   end

signature QUASIGROUP =
   sig
      include MAGMA

      (* Laws:
       *   (y / x) * x = y
       *   (y * x) / x = y
       *   x * (x \ y) = y
       *   x \ (x * y) = y
       *)
      val / : t * t -> t
      val \ : t * t -> t
   end

signature LOOP =
   sig
      include QUASIGROUP

      (* Laws:
       *   x * one = one * x = x
       *)
      val one: t
   end

signature LEFT_BOL_LOOP =
   sig
      include LOOP

      (* Laws:
       *   x * (y * (x * z)) = (x * (y * x)) * z
       *)
   end

signature RIGHT_BOL_LOOP =
   sig
      include LOOP

      (* Laws:
       *   ((z * x) * y) * x = z * ((x * y) * x)
       *)
   end

signature MOUFANG_LOOP =
   sig
      include LOOP

      (* Laws:
       *   x * (y * (x * z)) = (x * (y * x)) * z
       *   ((z * x) * y) * x = z * ((x * y) * x)
       *
       *   Equivalently, any one of the following hold:
       *   x * (y * (x * z)) = ((x * y) * x) * z
       *   z * (x * (y * x)) = ((z * x) * y) * x
       *   (x * y) * (z * x) = x * ((y * z) * x)
       *   (x * y) * (z * x) = (x * (y * z)) * x
       *)
   end

signature GROUP =
   sig
      include MOUFANG_LOOP

      val inv: t -> t
   end

(* vim: set ts=3 sw=3 :*)
