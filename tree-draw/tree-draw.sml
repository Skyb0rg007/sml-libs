
structure TreeDraw =
struct
  datatype 'a t = Node of 'a * 'a t list

  type extent = (real * real) list

  fun moveT (Node ((lbl, x), ts), x') = Node ((lbl, Real.+ (x, x')), ts)

  fun moveE (e, x) = List.map (fn (p, q) => (Real.+ (p, x), Real.+ (q, x))) e

  fun merge ([], qs) = qs
    | merge (ps, []) = ps
    | merge ((p, _) :: ps, (_, q) :: qs) = (p, q) :: merge (ps, qs)

  fun mergeList es = List.foldr merge [] es

  fun fit ((_, p) :: ps, (q, _) :: qs) = Real.max (fit (ps, qs), p - q + 1.0)
    | fit _ = 0.0

  fun fitListl es =
    let
      fun go (acc, []) = []
        | go (acc, e :: es) =
          let
            val x = fit (acc, e)
          in
            x :: go (merge (acc, moveE (e, x)), es)
          end
    in
      go ([], es)
    end

  fun fitListr es =
    let
      fun go (acc, []) = []
        | go (acc, e :: es) =
          let
            val x = ~(fit (e, acc))
          in
            x :: go (merge (moveE (e, x), acc), es)
          end
    in
      List.rev (go ([], List.rev es))
    end

  fun fitList es = List.map (fn (a, b) => (a + b) / 2.0) (ListPair.zipEq (fitListl es, fitListr es))

  val _: ('a * real) t * real -> ('a * real) t = moveT
  val _: extent * real -> extent = moveE
  val _: extent * extent -> extent = merge
  val _: extent list -> extent = mergeList
  val _: extent * extent -> real = fit
  val _: extent list -> real list = fitListl
  val _: extent list -> real list = fitListr
  val _: extent list -> real list = fitList

  fun design t =
    let
      fun go (Node (lbl, ts)) =
        let
          val (ts, es) = ListPair.unzip (List.map go ts)
          val ps = fitList es
          val pts = List.map moveT (ListPair.zip (ts, ps))
          val pes = List.map moveE (ListPair.zip (es, ps))
          val t = Node ((lbl, 0.0), pts)
          val e = (0.0, 0.0) :: mergeList pes
        in
          (t, e)
        end
    in
      #1 (go t)
    end

  val _: 'a t -> ('a * real) t = design
end
