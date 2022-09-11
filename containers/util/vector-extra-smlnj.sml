
structure VectorEx =
struct

open Vector

fun create n =
   let
      val arr = Array.array (n, NONE)

      fun sub i = Option.valOf (Array.sub (arr, i))

      fun done () = Vector.tabulate (n, sub)

      fun update (i, x) = Array.update (arr, i, SOME x)
   in
      {done = done, sub = sub, update = update}
   end

fun unfoldli (n, b, f) =
   let
      val {done, update, ...} = create n

      fun go (i, acc) =
         if i >= n
            then (done (), acc)
         else
            let
               val (x, acc) = f (i, acc)
            in
               update (i, x)
               ; go (i + 1, acc)
            end
   in
      go (0, b)
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
