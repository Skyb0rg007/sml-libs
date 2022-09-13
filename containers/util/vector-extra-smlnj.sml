
structure VectorEx: VECTOR_EXTRA =
struct

fun create n =
   let
      val arr = Array.array (n, NONE)
      val updateLim = ref 0
      val gotIt = ref false

      fun sub i =
         case Array.sub (arr, i) of
            NONE => raise Subscript
          | SOME x => x

      fun update (i, x) =
         if !gotIt
            then raise Fail "cannot update after calling done"
         else if i < !updateLim
            then Array.update (arr, i, SOME x)
         else if i = !updateLim andalso i < n
            then (Array.update (arr, i, SOME x); updateLim := i + 1)
         else raise Subscript

      fun done () =
         if !gotIt
            then raise Fail "already got vector"
         else if n = !updateLim
            then (gotIt := true; Vector.tabulate (n, sub))
         else raise Fail "vector not full"
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
