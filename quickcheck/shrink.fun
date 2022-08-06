
functor ShrinkNumberFn(N: SHRINK_NUMBER) :> SHRINK where type t = N.t =
struct
   type t = N.t

   fun shrink {dest} x =
      let
         fun go cur =
            if N.compare (cur, x) = EQUAL
               then NONE
            else
               let
                  val halfx = N.div (x, N.fromInt 2)
                  val halfcur = N.div (cur, N.fromInt 2)
                  val halfDiff = N.- (halfx, halfcur)
               in
                  if N.compare (halfDiff, N.fromInt 0) = EQUAL
                     then SOME (cur, x)
                  else SOME (cur, N.+ (cur, halfDiff))
               end
      in
         Seq.unfold go dest
      end

   fun shrinkAggressive {dest} x =
      let
         fun go cur =
            case N.compare (cur, x) of
               EQUAL => NONE
             | LESS =>
                  let
                     val next = N.+ (cur, N.fromInt 1)
                  in
                     SOME (next, next)
                  end
             | GREATER =>
                  let
                     val next = N.- (cur, N.fromInt 1)
                  in
                     SOME (next, next)
                  end
      in
         Seq.unfold go dest
      end
end

(* vim: set tw=0 ts=3 sw=3: *)
