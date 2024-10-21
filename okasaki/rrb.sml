
signature RRB =
   sig
   end

structure RRB =
   struct
      structure V = Vector
      structure VS = VectorSlice
      structure A = Array
      structure AS = ArraySlice

      structure VS =
         struct
            open VS

            fun update (slice, i, x) =
               full (V.update (vector slice, i, x))

            fun adjust (slice, i, f) =
               let
                  val v = vector slice
               in
                  full (V.update (v, i, f (V.sub (v, i))))
               end

            fun append (s1, s2) =
               let
                  val len1 = length s1
                  val len2 = length s2
                  fun f i =
                     if i < len1
                        then sub (s1, i)
                     else sub (s2, i - len1)
               in
                  full (V.tabulate (len1 + len2, f))
               end
         end

      structure Buffer =
         struct
            datatype 'a t = Buffer of 'a array * int ref

            fun new (cap, x) = Buffer (A.array (cap, x), ref 0)

            fun push (Buffer (a, i), x) =
               (A.update (a, !i, x);
                i := !i + 1)

            fun get (Buffer (a, i)) =
               AS.vector (AS.slice (a, 0, SOME (!i)))
               before i := 0

            fun size (Buffer (_, i)) = !i

            fun isFull (Buffer (a, i)) = A.length a = !i
         end

      datatype 'a t = Empty
                    | Root of { size : int, shift : word, tree : 'a tree }

      and 'a tree = Balanced of 'a tree VS.slice
                  | Unbalanced of 'a tree VS.slice * int V.vector
                  | Leaf of 'a VS.slice

      infix >> << andb
      val op andb = Word.andb
      val op >> = Word.>>
      val op << = Word.<<

      val blockSize = 16
      val blockShift = 0w4
      val blockMask = 0wxf

      fun treeSize (shift, tree) =
         let
            fun go (acc, _, Leaf arr) = acc + VS.length arr
              | go (acc, _, Unbalanced (arr, sizes)) =
               acc + V.sub (sizes, VS.length arr - 1)
              | go (acc, shift, Balanced arr) =
               let
                  val i = VS.length arr - 1
                  val acc' = acc + Word.toInt (Word.fromInt i << shift)
               in
                  go (acc', shift - blockShift, VS.sub (arr, i))
               end
         in
            go (0, shift, tree)
         end

      fun radixIndex (i, shift) =
         Word.toInt ((Word.fromInt i >> shift) andb blockMask)

      fun relaxedRadixIndex (sizes, i, shift) =
         let
            val guess = radixIndex (i, shift)
            fun loop idx =
               if i < Vector.sub (sizes, idx)
                  then idx
               else loop (idx + 1)
            val idx = loop guess
            val subIdx = if idx = 0 then i else i - Vector.sub (sizes, idx - 1)
         in
            (idx, subIdx)
         end

      fun size Empty = 0
        | size (Root {size, ...}) = size

      val empty = Empty

      fun singleton x = Root {
            size = 1,
            shift = 0w0,
            tree = Leaf (VS.full #[x])
         }

      fun fromList [] = Empty
        | fromList [x] = singleton x
        | fromList (xs as def :: _) =
         let
            fun nodes def f =
               let
                  val buffer = Buffer.new (blockSize, def)

                  fun go [] = [f (Buffer.get buffer)]
                    | go (t :: ts) =
                     if Buffer.isFull buffer
                        then
                           let
                              val result = Buffer.get buffer
                           in
                              Buffer.push (buffer, t);
                              f result :: go ts
                           end
                     else (Buffer.push (buffer, t); go ts)
               in
                  go
               end

            val leafNodes = nodes (VS.full #[]) (Leaf o VS.full)
            val balNodes = nodes (Leaf (VS.full #[])) (Balanced o VS.full)

            fun iterateNodes (shift, trees) =
               case balNodes trees of
                  [t] => Root { size = treeSize (shift, t), shift = shift, tree = t }
                | ts => iterateNodes (shift + blockShift, ts)
         in
            case leafNodes xs of
               [t] => Root { size = treeSize (0w0, t), shift = 0w0, tree = t }
             | ts => iterateNodes (blockShift, ts)
         end

      fun sub (Empty, _) = raise Subscript
        | sub (Root {size, shift, tree}, i) =
         let
            fun go (i, _, Leaf arr) =
               VS.sub (arr, Word.toInt (Word.fromInt i andb blockMask))
              | go (i, shift, Balanced arr) =
               go (i, shift - blockShift, VS.sub (arr, radixIndex (i, shift)))
              | go (i, shift, Unbalanced (arr, sizes)) =
               let
                  val (idx, subIdx) = relaxedRadixIndex (sizes, i, shift)
               in
                  go (subIdx, shift - blockShift, VS.sub (arr, idx))
               end
         in
            if i < 0 orelse i >= size
               then raise Subscript
            else go (i, shift, tree)
         end

      fun toList v =
         List.tabulate (size v, fn i => sub (v, i))

      fun update (Empty, _, _) = raise Subscript
        | update (Root {size, shift, tree}, i, x) =
         let
            fun go (i, _) (Leaf arr) =
               Leaf (VS.update (arr, Word.toInt (Word.fromInt i andb blockMask), x))
              | go (i, shift) (Balanced arr) =
               Balanced (VS.adjust (arr, radixIndex (i, shift), go (i, shift - blockShift)))
              | go (i, shift) (Unbalanced (arr, sizes)) =
               let
                  val (idx, subIdx) = relaxedRadixIndex (sizes, i, shift)
               in
                  Unbalanced (VS.adjust (arr, idx, go (subIdx, shift - blockShift)), sizes)
               end
         in
            if i < 0 orelse i >= size
               then raise Subscript
            else Root {size = size, shift = shift, tree = go (i, shift) tree}
         end

      fun treeToArray (Balanced arr) = arr
        | treeToArray (Unbalanced (arr, _)) = arr
        | treeToArray (Leaf _) = raise Fail "treeToArray"

      fun isBalanced _ = raise Fail ""

      fun computeSizes shift arr =
         if isBalanced (shift, arr)
            then Balanced arr
         else raise Fail ""

      fun mergeRebalance' _ = raise Fail ""

      fun mergeRebalance (shift, left, center, right) =
         if shift = blockShift
            then mergeRebalance' (fn Leaf arr => arr) Leaf
         else mergeRebalance' treeToArray (computeSizes (shift - blockShift))

      fun mergeTrees (shift1, Leaf arr1, shift2, Leaf arr2) =
         if VS.length arr1 = blockSize
            then VS.full #[Leaf arr1, Leaf arr2]
         else if VS.length arr1 + VS.length arr2 <= blockSize
            then VS.full #[Leaf (VS.append (arr1, arr2))]
         else
            let
               val (left, right) = VS.splitAt (VS.append (arr1, arr2), blockSize)
            in
               VS.full #[Leaf left, Leaf right]
            end
        | mergeTrees (shift1, tree1, shift2, tree2) =
         case Word.compare (shift1, shift2) of
            LESS =>
               let
                  val right = treeToArray tree1
                  val rightHead = VS.sub (right, 0)
                  val rightTail = VS.subslice (right, 1, NONE)
                  val merged = mergeTrees (shift1, tree1, shift2 - blockShift, rightHead)
               in
                  mergeRebalance (shift2, VS.full #[], merged, rightTail)
               end

      (* fun append (Empty, v) = v *)
      (*   | append (v, Empty) = v *)
      (*   | append (Root {size = size1, shift = shift1, tree = tree1}, *)
      (*             Root {size = size2, shift = shift2, tree = tree2}) = *)
      (*    let *)
      (*       val maxShift = Word.max (shift1, shift2) *)
            
      (*    in *)
      (*       Empty *)
      (*    end *)
   end

(* vim: set ts=3 sw=3: *)
