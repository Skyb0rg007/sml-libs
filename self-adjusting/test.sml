
functor Test(structure P: PROPOGATE) =
struct

fun sum (arr, res) =
   let
      fun go (lo, hi, res) =
         if lo = hi - 1
            then P.read ([Vector.sub (arr, lo)], fn [x] => P.write (res, x)
                                                  | _ => raise Fail "Impossible")
         else
            let
               val mid = lo + (hi - lo) div 2
               val left = P.alloc ()
               val right = P.alloc ()
            in
               P.par (fn () => go (lo, mid, left),
                      fn () => go (mid, hi, right))
               ; P.read ([left, right], fn [x, y] => P.write (res, x + y)
                                         | _ => raise Fail "Impossible")
            end
   in
      go (0, Vector.length arr, res)
   end

val vec: int P.t vector = Vector.tabulate (10, fn _ => P.alloc ())
val () = Vector.appi (fn (i, r) => P.write (r, i)) vec

(* val res = P.alloc () *)



end

(* vim: set tw=0 ts=3 sw=3: *)
