
signature EITHER =
   sig
      datatype ('a, 'b) either = INL of 'a | INR of 'b

      val isLeft: ('a, 'b) either -> bool
      val isRight: ('a, 'b) either -> bool
      val asLeft: ('a, 'b) either -> 'a option
      val asRight: ('a, 'b) either -> 'b option

      val map: ('a -> 'c) * ('b -> 'd) -> ('a, 'b) either -> ('c, 'd) either
      val app: ('a -> unit) -> ('b -> unit) -> ('a, 'b) either -> unit
      val fold: ('a * 'c -> 'c) -> ('b * 'c -> 'c) -> 'c -> ('a, 'b) either -> 'c
      val proj: ('a, 'a) either -> 'a
      val partition: ('a, 'b) either list -> 'a list * 'b list
      val mapLeft: ('a -> 'c) -> ('a, 'b) either -> ('c, 'b) either
      val mapRight: ('b -> 'c) -> ('a, 'b) either -> ('a, 'c) either
      val appLeft: ('a -> unit) -> ('a, 'b) either -> unit
      val appRight: ('b -> unit) -> ('a, 'b) either -> unit
   end

structure Either: EITHER =
   struct
      datatype ('a, 'b) either = INL of 'a | INR of 'b

      fun isLeft (INL _) = true
        | isLeft (INR _) = false

      fun isRight (INL _) = false
        | isRight (INR _) = true

      fun asLeft (INL a) = SOME a
        | asLeft (INR _) = NONE

      fun asRight (INL _) = NONE
        | asRight (INR b) = SOME b

      fun map (f, _) (INL a) = INL (f a)
        | map (_, g) (INR b) = INR (g b)

      fun app f _ (INL a) = f a
        | app _ g (INR b) = g b

      fun fold f _ z (INL a) = f (a, z)
        | fold _ g z (INR b) = g (b, z)

      fun proj (INL a) = a
        | proj (INR a) = a

      fun partition xs =
         let
            fun go (INL a, (l, r)) = (a :: l, r)
              | go (INR b, (l, r)) = (l, b :: r)
         in
            List.foldr go ([], []) xs
         end

      fun mapLeft f (INL a) = INL (f a)
        | mapLeft _ (INR b) = INR b

      fun mapRight _ (INL a) = INL a
        | mapRight g (INR b) = INR (g b)

      fun appLeft f (INL a) = f a
        | appLeft _ (INR _) = ()

      fun appRight _ (INL _) = ()
        | appRight g (INR b) = g b
   end

(* vim: set tw=0 ts=3 sw=3: *)
