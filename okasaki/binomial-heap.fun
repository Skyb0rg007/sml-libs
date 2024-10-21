
signature ORDERED =
   sig
      type t

      val <= : t * t -> bool
   end

functor BinomialHeapFn(T : ORDERED) :> HEAP where type elt = T.t =
   struct
      datatype tree = Node of int * elt * t

      withtype t = tree list

      and elt = T.t

      exception Empty

      fun rank (Node (r, _, _)) = r
      fun root (Node (_, x, _)) = x

      fun link (t1 as Node (r, x1, c1), t2 as Node (_, x2, c2)) =
         if T.<= (x1, x2)
            then Node (r + 1, x1, t2 :: c1)
            else Node (r + 1, x2, t1 :: c2)

      fun insertTree (ts, t) =
         case ts of
            [] => [t]
          | t1 :: rest =>
               if rank t < rank t1
                  then t :: ts
                  else insertTree (rest, link (t1, t))

      val empty = []

      val isEmpty = List.null

      fun insert (ts, x) = insertTree (ts, Node (0, x, []))

      fun merge (ts, []) = ts
        | merge ([], ts) = ts
        | merge (t1 :: ts1, t2 :: ts2) =
         case Int.compare (rank t1, rank t2) of
            LESS => t1 :: merge (ts1, t2 :: ts2)
          | GREATER => t2 :: merge (t1 :: ts1, ts2)
          | EQUAL => insertTree (merge (ts1, ts2), link (t1, t2))

      fun findMin [] = raise Empty
        | findMin [t] = root t
        | findMin (t :: ts) =
         let
            val x = root t
            val y = findMin ts
         in
            if T.<= (x, y) then x else y
         end

      local
         fun findMinTree [] = raise Empty
           | findMinTree [t] = (t, [])
           | findMinTree (t :: ts) =
            let
               val (t', ts') = findMinTree ts
            in
               if T.<= (root t, root t')
                  then (t, ts)
                  else (t', t :: ts')
            end
      in
         fun deleteMin ts =
            let
               val (Node (_, x, ts1), ts2) = findMinTree ts
            in
               merge (List.rev ts1, ts2)
            end
      end
   end

(* vim: set ts=3 sw=3: *)
