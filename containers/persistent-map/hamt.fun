
functor HAMTFn(Key: HASH_KEY) =
struct

type key = Key.hash_key

val hashKey = Key.hashVal
val sameKey = Key.sameKey

datatype 'a node =
   Empty
 | BitmapIndexed of word * 'a node vector
 | Full of 'a node vector
 | Leaf of word * key * 'a
 | Collision of word * (key * 'a) vector

datatype 'a map = Map of {tree: 'a node, size: int}

(** Helpers **)

structure Vector =
   struct
      open Vector

      fun insert (v, i, x) =
         let
            fun f idx =
               case Int.compare (idx, i) of
                  LESS => sub (v, idx)
                | EQUAL => x
                | GREATER => sub (v, idx - 1)
         in
            tabulate (length v + 1, f)
         end

      fun delete (v, i) =
         let
            fun f idx =
               if idx < i
                  then sub (v, idx)
               else sub (v, idx + 1)
         in
            tabulate (length v - 1, f)
         end

      fun snoc (v, x) =
         let
            val len = length v
            fun f idx =
               if idx = len
                  then x
               else sub (v, idx)
         in
            tabulate (length v + 1, f)
         end

      fun liftEqual eq (v1, v2) =
         let
            val len1 = length v1
            val len2 = length v2
            fun go i =
               i = len1
               orelse (eq (sub (v1, i), sub (v2, i)) andalso go (i + 1))
         in
            len1 = len2 andalso go 0
         end

      (* fun isPermutationWith eq (v1, v2) = *)
      (*    let *)
      (*       fun removeWith eq (v, x) = *)
      (*          case findi (fn (_, y) => eq (x, y)) v of *)
      (*             NONE => v *)
      (*           | SOME (k, _) => delete (v, k) *)

      (*       val len1 = length v1 *)
      (*       val len2 = length v2 *)

      (*       fun go (i, j) = *)
      (*          if i = len1 andalso j = len2 *)
      (*             then true *)
      (*          else if i = len1 *)
      (*             then false *)
      (*          else if j = len2 *)
      (*             then false *)
      (*          else *)
      (*             let *)
      (*                val x = sub (v, i) *)
      (*                val y = sub (v, j) *)
      (*             in *)
      (*             end *)
      (*    in *)
      (*       v1 *)
      (*    end *)
   end

val bitsPerSubkey = 0w5
val subkeyMask = 0wx1f
val maxChildren = 0w32
val fullNodeMask = 0wxffffffff

fun index (w, s) = Word.andb (Word.>> (w, s), subkeyMask)
fun index' (w, s) = Word.toInt (index (w, s))
fun mask (w, s) = Word.<< (0w1, index (w, s))
fun sparseIndex (b, m) = Word.popCount (Word.andb (b, m - 0w1))

fun two (s, (h, k, x), (h', k', x')) =
   let
      val leaf = Leaf (h, k, x)
      val leaf' = Leaf (h', k', x')

      fun go s =
         let
            val b = mask (h, s)
            val b' = mask (h', s)
         in
            if b = b'
               then BitmapIndexed (b, #[go (s + bitsPerSubkey)])
            else if index (h, s) < index (h', s)
               then BitmapIndexed (Word.orb (b, b'), #[leaf, leaf'])
            else BitmapIndexed (Word.orb (b, b'), #[leaf', leaf])
         end
   in
      go s
   end

(** Exports **)

val empty = Map {tree = Empty, size = 0}

fun singleton (k, x) = Map {tree = Leaf (hashKey k, k, x), size = 1}

fun insertLookupWithi f (Map {tree, size}, k, x) =
   let
      val h = hashKey k

      fun go (_, Empty) = (Leaf (h, k, x), NONE)
        | go (s, Leaf (h', k', x')) =
         if h = h'
            then
               if sameKey (k, k')
                  then (Leaf (h, k, f (k, x', x)), SOME x')
               else (Collision (h, #[(k', x'), (k, x)]), NONE)
         else (two (s, (h, k, x), (h', k', x')), NONE)
        | go (s, Collision (h', kxs)) =
         if h = h'
            then raise Fail "NYI"
         else raise Fail "NYI"
        | go (s, _) = raise Fail "NYI"

      val (tree', x') = go (0w0, tree)
      val size' = if Option.isSome x' then size else size + 1
   in
      (Map {tree = tree', size = size'}, x')
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
