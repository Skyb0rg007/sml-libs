
structure Sort =
struct
  fun tournamentSort op <= xs =
    let
      datatype 'a tree = Node of 'a * 'a tree list

      fun game (Node (x, xs), Node (y, ys)) =
        if x <= y
          then Node (x, Node (y, ys) :: xs)
        else Node (y, Node (x, xs) :: ys)

      fun round ([], done) = done
        | round ([t], done) = t :: done
        | round (t :: t' :: ts, done) = round (ts, game (t, t') :: done)

      fun tournament [t] = t
        | tournament ts = tournament (round (ts, []))

      fun go [] = []
        | go ts =
        case tournament ts of
           Node (x, xs) => x :: go xs
    in
      go (List.map (fn x => Node (x, [])) xs)
    end

  (* The Haskell version *)
  fun mergeSort op <= xs =
    let
      fun sequences (x :: y :: xs) =
            if x <= y
              then ascending (y, fn zs => x :: zs, xs)
            else descending (y, [x], xs)
        | sequences xs = [xs]

      and descending (x, xs, y :: ys) =
            if x <= y
              then (x :: xs) :: sequences (y :: ys)
            else descending (y, x :: xs, ys)
        | descending (x, xs, ys) = (x :: xs) :: sequences ys

      and ascending (x, xs, y :: ys) =
            if x <= y
              then ascending (y, fn zs => xs (x :: zs), ys)
            else xs [x] :: sequences (y :: ys)
        | ascending (x, xs, ys) = xs [x] :: sequences ys

      fun merge ([], ys) = ys
        | merge (xs, []) = xs
        | merge (x :: xs, y :: ys) =
            if x <= y
              then x :: merge (xs, y :: ys)
            else y :: merge (x :: xs, ys)

      fun mergePairs (x :: y :: xs) = merge (x, y) :: mergePairs xs
        | mergePairs xs = xs

      fun mergeAll [x] = x
        | mergeAll xs = mergeAll (mergePairs xs)
    in
      mergeAll (sequences xs)
    end


  fun shellSort op <= xs =
    let
      val gaps = #[701, 301, 132, 57, 23, 10, 4, 1]
      val arr = Array.fromList xs
      val len = Array.length arr

      fun loop2 (gap, j, tmp) =
        if j < gap orelse Array.sub (arr, j - gap) <= tmp
          then Array.update (arr, j, tmp)
        else (Array.update (arr, j, Array.sub (arr, j - gap))
              ; loop2 (gap, j - gap, tmp))

      fun loop1 (gap, i) =
        if i < len
          then (loop2 (gap, i, Array.sub (arr, i)); loop1 (gap, i + 1))
        else ()

      val () = Vector.app (fn gap => loop1 (gap, gap)) gaps
    in
      Array.toList arr
    end

  structure ArrayExtras =
    struct
      fun swap (arr, i, j) =
        let
          val x = Array.sub (arr, i)
          val y = Array.sub (arr, j)
        in
          Array.update (arr, i, y);
          Array.update (arr, j, x)
        end
    end

  structure ArraySliceExtras =
    struct
      fun reverse slice =
        let
          val n = ArraySlice.length slice
          val lim = Int.quot (n - 2, 2)
          val () = print ("lim = " ^ Int.toString lim ^ "\n")
          fun loop i =
            if i > lim
              then ()
              else
                let
                  val tmp = ArraySlice.sub (slice, i)
                  val j = n - 1 - i
                in
                  ArraySlice.update (slice, i, ArraySlice.sub (slice, j));
                  ArraySlice.update (slice, j, tmp);
                  loop (i + 1)
                end
        in
          loop 0
        end
    end

  fun insertionSort op <= slice =
    let
      val len = ArraySlice.length slice

      fun outer i =
        if i >= len
          then ()
        else
          let
            val x = ArraySlice.sub (slice, i)
            fun inner j =
              if j < 0
                then ArraySlice.update (slice, j + 1, x)
              else
                let
                  val y = ArraySlice.sub (slice, j)
                in
                  if y <= x
                    then ArraySlice.update (slice, j + 1, x)
                  else
                    (ArraySlice.update (slice, j + 1, y);
                     inner (j - 1))
                end
          in
            inner (i - 1);
            outer (i + 1)
          end
    in
      outer 1
    end

  fun wikiSort op <= arr =
    let
      fun floorPow2 w =
        let
          val w = Word.orb (w, Word.>> (w, 0w1))
          val w = Word.orb (w, Word.>> (w, 0w2))
          val w = Word.orb (w, Word.>> (w, 0w4))
          val w = Word.orb (w, Word.>> (w, 0w8))
          val w = Word.orb (w, Word.>> (w, 0w16))
          val w = Word.orb (w, Word.>> (w, 0w32))
        in
          Word.- (w, Word.>> (w, 0w1))
        end

      fun swap (i, j) =
        let
          val x = Array.sub (arr, i)
          val y = Array.sub (arr, j)
        in
          Array.update (arr, j, x);
          Array.update (arr, i, y)
        end

      fun reverse (i, n) =
        let
          val lim = Int.quot (n - 2, 2)
          fun loop i =
            if i <= lim
              then
                let
                  val () = print ("i = " ^ Int.toString i ^ "\n")
                  val () = print ("lim = " ^ Int.toString lim ^ "\n")
                  val j = n - 1 - i
                  val tmp = Array.sub (arr, i)
                in
                  Array.update (arr, i, Array.sub (arr, j));
                  Array.update (arr, j, tmp);
                  loop (i + 1)
                end
              else ()
        in
          loop i
        end

      fun blockSwap (i, n, m) =
        (reverse (i, n);
         reverse (i + n, m);
         reverse (i, m))

      fun rotate (blockSize, i, n) =
        (reverse (i, n);
         reverse (i, blockSize);
         reverse (i + blockSize, n - blockSize))
    in
      {reverse = reverse,
       rotate = rotate,
       swap = swap}
    end

  val rng = ref (Random.rand (0, 0))
  fun randomInts () =
    List.tabulate (100000, fn _ => Random.randInt (!rng))

  fun isSorted op <= (x :: y :: ys) = x <= y andalso isSorted op <= (y :: ys)
    | isSorted _ [x] = true
    | isSorted _ [] = true

  fun valid sort =
    let
      fun go 0 = true
        | go n = isSorted Int.<= (sort Int.<= (randomInts ()))
                 andalso go (n - 1)
    in
      go 1000
    end

  fun time sort =
    let
      val () = rng := Random.rand (0, 0)

      fun test () = ignore (sort (randomInts ()))

      fun go 0 = ()
        | go n = (test (); go (n - 1))

      val t = Timer.startCPUTimer ()
      val () = go 1000
      val {sys, usr} = Timer.checkCPUTimer t
    in
      print ("sys: " ^ Time.toString sys ^ "\n");
      print ("usr: " ^ Time.toString usr ^ "\n")
    end
end
