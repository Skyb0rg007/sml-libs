
signature PERSISTENT_VECTOR =
   sig
   end

structure PersistentVector =
   struct
      datatype 'a t = Vector of {
            count : int,
            shift : word,
            root : 'a node
         }

      and 'a node = Node of 'a node vector
                  | Leaf of 'a vector

      fun radixIndex (level, idx) =
         Word.toInt (Word.andb (Word.>> (Word.fromInt idx, level), 0wx1f))

      (** **)

      fun length (Vector {count, ...}) = count

      fun foldl f z (Vector {root, ...}) =
         let
            fun go (Node arr, acc) = Vector.foldl go acc arr
              | go (Leaf arr, acc) = Vector.foldl f acc arr
         in
            go (root, z)
         end

      fun foldr f z (Vector {root, ...}) =
         let
            fun go (Node arr, acc) = Vector.foldr go acc arr
              | go (Leaf arr, acc) = Vector.foldr f acc arr
         in
            go (root, z)
         end

      fun foldli f z (Vector {root, ...}) =
         let
            fun f' (i, acc) = (i + 1, f (i, acc))

            fun go (Node arr, (i, acc)) = Vector.foldl go (i, acc) arr
              | go (Leaf arr, (i, acc)) = Vector.foldl f' (i, acc) arr
         in
            #2 (go (root, (0, z)))
         end

      fun foldri f z (Vector {root, ...}) =
         let
            fun f' (i, acc) = (i + 1, f (i, acc))

            fun go (Node arr, (i, acc)) = Vector.foldr go (i, acc) arr
              | go (Leaf arr, (i, acc)) = Vector.foldr f' (i, acc) arr
         in
            #2 (go (root, (0, z)))
         end

      fun sub (Vector {count, shift, root}, i) =
         let
            fun go (shift, Node arr) = go (shift - 0w5, Vector.sub (arr, radixIndex (shift, i)))
              | go (_, Leaf arr) = Vector.sub (arr, radixIndex (0w0, i))
         in
            go (shift, root)
         end

      fun update (Vector {count, shift, root}, i, x) =
         let
            fun go (shift, node) = 
               let
                  val idx = radixIndex (shift, i)
               in
                  case node of
                     Node arr => Node (Vector.update (arr, idx, go (shift - 0w5, Vector.sub (arr, idx))))
                   | Leaf arr => Leaf (Vector.update (arr, idx, x))
               end
         in
            if 0 <= i andalso i < count
               then
                  Vector {
                     count = count,
                     shift = shift,
                     root = go (shift, root)
                  }
            else raise Subscript
         end

      fun map f (Vector {count, shift, root}) =
         let
            fun go (Node arr) = Node (Vector.map go arr)
              | go (Leaf arr) = Leaf (Vector.map f arr)
         in
            Vector {
               count = count,
               shift = shift,
               root = go root
            }
         end

      fun head (Vector {count, root, ...}) =
         let
            fun go (Node arr) = go (Vector.sub (arr, 0))
              | go (Leaf arr) = Vector.sub (arr, 0)
         in
            if count = 0
               then NONE
            else SOME (go root)
         end

      (* fun edit (Node (e, _)) = e *)
      (*   | edit (Leaf (e, _)) = e *)

      (* fun pushTail (level, parent, tailnode) = *)
      (*    let *)
      (*       val subidx = levelIndex (level, ) *)
      (*    in *)
      (*    end *)

      (* fun tailoff (v as Vector {count, ...}) = *)
      (*    if count < 32 *)
      (*       then 0 *)
      (*    else *)
      (*       Word.toInt (Word.<< (Word.>> (Word.fromInt (count - 1), 0w5), 0w5)) *)

      (* fun arrayFor (v as Vector {count, shift, root, tail}, i) = *)
      (*    if 0 <= i andalso i < count *)
      (*       then *)
      (*          if i >= tailoff v *)
      (*             then tail *)
      (*          else *)
      (*             let *)
      (*                fun go (Node (_, arr), level) = *)
      (*                   go (Array.sub (arr, levelIndex (level, i)), level - 0w5) *)
      (*                  | go (Leaf (_, arr), _) = arr *)
      (*             in *)
      (*                go (root, shift) *)
      (*             end *)
      (*    else raise Subscript *)

      (* fun sub (v, i) = *)
      (*    let *)
      (*       val arr = arrayFor (v, i) *)
      (*    in *)
      (*       Array.sub (arr, levelIndex (0w0, i)) *)
      (*    end *)

      (* fun cons (x, v as Vector {count, shift, root, tail}) = *)
      (*    if count - tailoff v < 32 *)
      (*       then *)
      (*          let *)
      (*             val len = Array.length tail *)
      (*             val newtail = Array.array (len + 1, Array.sub (tail, 0)) *)
      (*          in *)
      (*             Array.copy { src = tail, dst = newtail, di = 0 }; *)
      (*             Array.update (newtail, len, x); *)
      (*             Vector { *)
      (*                count = count + 1, *)
      (*                shift = shift, *)
      (*                root = root, *)
      (*                tail = newtail *)
      (*             } *)
      (*          end *)
      (*    else *)
      (*       let *)
      (*          val overflow = *)
      (*             Word.>> (Word.fromInt count, 0w5) > Word.<< (0w1, shift) *)
      (*          val tailnode = Node (edit root, tail) *)

      (*          val (newshift, newroot) = *)
      (*             if overflow *)
      (*                then raise Option *)
      (*             else (shift, pushTail (shift, root, tailnode)) *)
      (*       in *)
      (*          Vector { *)
      (*             count = count + 1, *)
      (*             shift = newshift, *)
      (*             root = newroot, *)
      (*             tail = Array.fromList [x] *)
      (*          } *)
      (*       end *)
   end

(* vim: set ts=3 sw=3: *)
