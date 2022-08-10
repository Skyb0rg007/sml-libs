
signature LAZY_SHARING_PAIR_STRUCTS =
sig
   structure A:
      sig
         type t

         val hash: t -> word
         val same: t * t -> bool
      end

   structure B:
      sig
         type t

         val hash: t -> word
         val same: t * t -> bool
      end
end

signature LAZY_SHARING_PAIR =
sig
   include LAZY_SHARING_PAIR_STRUCTS

   type t

   val make: A.t * B.t -> t
   val car: t -> A.t
   val cdr: t -> B.t
   val same: t * t -> bool
end

functor LazySharingPairFn(S: LAZY_SHARING_PAIR_STRUCTS): LAZY_SHARING_PAIR =
struct
   structure A = S.A
   structure B = S.B
   structure DS = DisjointSet

   datatype t = T of {
      car: A.t DS.t,
      cdr: B.t DS.t,
      hash: word
   }

   (* This code is copied from boost::hash_combine *)
   fun hashCombine (w1, w2) =
      let
         open Word
         infix xorb >> <<
         val magic = 0wx9e3779b9
      in
         w1 xorb (w2 + magic + (w1 << 0w6) + (w1 >> 0w2))
      end

   fun make (a, b) = T {car = DS.make a, cdr = DS.make b, hash = hashCombine (A.hash a, B.hash b)}

   fun car (T {car, ...}) = DS.! car

   fun cdr (T {cdr, ...}) = DS.! cdr

   fun same (T p1, T p2) =
      if #hash p1 <> #hash p2
         (* Hashes don't match: they cannot be equal *)
         then false
      else if DS.same (#car p1, #car p2) andalso DS.same (#cdr p1, #cdr p2)
         (* Both references are the same: they must be equal *)
         then true
      else if not (A.same (DS.! (#car p1), DS.! (#car p2)))
         (* The `car` fields disagree: they are not equal *)
         then false
      else
         (* The `car` fields agree: merge them *)
         (DS.union (#car p1, #car p2)
          ; if not (B.same (DS.! (#cdr p1), DS.! (#cdr p2)))
               (* The `cdr` fields disagree: they are not equal *)
               then false
            else
               (* The `cdr` fields agree: merge them *)
               (DS.union (#cdr p1, #cdr p2)
                ; true))

end

structure P =
LazySharingPairFn(
   structure A =
      struct
         type t = int

         val hash = Word.fromInt
         val same: t * t -> bool = op =
      end

   structure B =
      struct
         type t = bool

         fun hash true = 0w1 | hash false = 0w2
         val same: t * t -> bool = op =
      end)

(* vim: set tw=0 ts=3 sw=3: *)
