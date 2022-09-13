
functor HashTableFn(Key: HASHABLE_TYPE): HASH_TABLE =
struct

type key = Key.t

datatype 'a table = T of {
   data: 'a bucketlist array ref,
   size: int ref,
   seed: word
}

and 'a bucketlist = Empty | Cons of key ref * 'a ref * 'a bucketlist ref

fun power2above (x, n) =
   if x >= n
      then x
   else power2above (x * 2, n)

(* Small rng implementation for hash table seeding *)
local
   val a = 0w16807
   val m = 0w2147483647
   val rng = ref 0w1
in
   fun rand () =
      let
         val x = a * !rng mod m
      in
         rng := x
         ; x
      end

   fun seed w = 
      let
         val w = w mod m
      in
         if w = 0w0
            then rng := 0w1
         else rng := w
      end

   (* Checked for SML/NJ *)
   (* val validation = 0w1043618065 *)
end


fun new n =
   let
      val n = power2above (16, n)
   in
      T {
         data = ref (Array.array (n, Empty)),
         size = ref 0,
         seed = rand ()
      }
   end

fun index (nbuckets, seed, key) =
   let
      (* Hash combining code from boost::hash_combine *)
      val h = Key.hash key
      val h = h + 0wx9e3779b9 + Word.<< (h, 0w6) + Word.>> (h, 0w2)
   in
      Word.toInt (Word.andb (h, Word.fromInt (nbuckets - 1)))
   end

fun resize (T {data, size, seed}) =
   let
      val odata = !data
      val osize = Array.length odata
      val nsize = osize * 2
      val ndata = Array.array (nsize, Empty)
      val ndata_tail = Array.array (nsize, Empty)

      fun insertBucket Empty = ()
        | insertBucket (cell as Cons (k, x, next)) =
         let
            val nidx = index (nsize, seed, !k)
         in
            case Array.sub (ndata_tail, nidx) of
               Empty => Array.update (ndata, nidx, cell)
             | Cons (_, _, next) => next := cell
            ; Array.update (ndata_tail, nidx, cell)
            ; insertBucket (!next)
         end
   in
      Array.app insertBucket odata
      ; Array.app
         (fn Empty => () | Cons (_, _, next) => next := Empty)
         ndata_tail
      ; data := ndata
      ; size := nsize
   end

fun find (T {data = ref data, seed, ...}, key) =
   let
      fun go Empty = NONE
        | go (Cons (k, x, rest)) =
         if Key.equals (key, !k)
            then SOME (!x)
         else go (!rest)
   in
      go (Array.sub (data, index (Array.length data, seed, key)))
   end

fun insert (tbl as T {data = ref data, size, seed}, key, value) =
   let
      fun go Empty = true
        | go (Cons (k, x, next)) =
         if Key.equals (key, !k)
            then (k := key; x := value; false)
         else go (!next)

      val idx = index (Array.length data, seed, key)
      val bkt = Array.sub (data, idx)
   in
      if go bkt
         then
            (Array.update (data, idx, Cons (ref key, ref value, ref bkt))
             ; size := !size + 1
             ; if !size > Array.length data * 2 then resize tbl else ())
      else ()
   end

fun foldi f acc (T {data = ref data, ...}) =
   let
      fun go (Empty, acc) = acc
        | go (Cons (k, x, next), acc) = go (!next, f (!k, !x, acc))

      val acc = ref acc
   in
      Array.app (fn bkt => acc := go (bkt, !acc)) data
      ; !acc
   end

fun toList tbl = foldi (fn (k, x, xs) => (k, x) :: xs) [] tbl

end

(* vim: set tw=0 ts=3 sw=3: *)
