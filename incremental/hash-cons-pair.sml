
signature HASH_CONS_PAIR_STRUCTS =
sig
   structure A:
      sig
         type t

         val hash: t -> word
         val same: t * t -> bool
         val ptrEq: t * t -> bool
      end

   structure B:
      sig
         type t

         val hash: t -> word
         val same: t * t -> bool
         val ptrEq: t * t -> bool
      end
end

functor HashConsPairFn(S: HASH_CONS_PAIR_STRUCTS) =
struct
   open S

   datatype t = T of {
      car: A.t ref,
      cdr: B.t ref
   }

   fun same (T {car = car1, cdr = cdr2}, T {car = car2, cdr = cdr2}) =
      let
         val a1 = !car1
         val a2 = !car2
         val b1 = !cdr1
         val b2 = !cdr2
      in
         if A.hash (!h1) <> A.hash (!h2)
            then false
         else if A.same (!h1, !h2)
            then true
         else if 
      end
end
(* vim: set tw=0 ts=3 sw=3: *)
