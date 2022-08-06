
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
