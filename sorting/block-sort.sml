
structure BlockSort =
struct

fun insertionSort cmp (arr, start, len) =
  let
    val stop = start + len

    fun outer i =
      if i < stop
        then inner (i, i, Array.sub (arr, i))
      else ()

    and inner (i, j, x) =
      if j <= start
        then finish (i, j, x)
      else
        let
          val y = Array.sub (arr, j - 1)
        in
          if cmp (x, y) = LESS
            then
              (Array.update (arr, j, y);
               inner (i, j - 1, x))
          else finish (i, j, x)
        end

    and finish (i, j, x) =
      (Array.update (arr, j, x);
       outer (i + 1))
  in
    outer (start + 1)
  end

end
