
functor WeakHashConsFn(Key: HASHABLE_TYPE):
   sig
      type key = Key.t
      type t

      val equals: t * t -> bool
      val node: t -> key
      val hashcons: key -> t
      val clear: unit -> unit
   end =
struct

type key = Key.t

datatype t = T of {
   hash: word,
   tag: word,
   node: Key.t
}

fun node (T {node, ...}) = node
fun equals (T {tag = t1, ...}, T {tag = t2, ...}) = t1 = t2

val initialSize = 2

val empty_bucket: t Weak.t array = Array.fromList []

fun tombstone n = T {hash = 0w0, tag = 0w0, node = n}
fun isTombstone (T {tag, ...}) = tag = 0w0

val g_table = ref (Array.array (initialSize, empty_bucket))
val g_totsize = ref 0
val g_limit = ref 3
val tag_counter = ref 0w1

fun nextTag () = !tag_counter before tag_counter := !tag_counter + 0w1

fun clear () =
   let
      val table = !g_table
      val len = Array.length table
      fun go i =
         if i < len
            then (Array.update (table, i, empty_bucket); go (i + 1))
         else ()
   in
      go 0
      ; g_totsize := 0
      ; g_limit := 3
   end

fun appTable f tbl =
   let
      fun go w =
         case Weak.get w of
            NONE => ()
          | SOME m => if isTombstone m then () else f m
   in
      Array.app (Array.app go) tbl
   end

fun resize () =
   let
      val old_table = !g_table
      val old_limit = !g_limit
      val old_len = Array.length old_table
      val new_len = 3 * old_len div 2 + 3
      val new_table = Array.array (new_len, empty_bucket)
   in
      g_totsize := 0
      ; g_limit := old_limit + 100
      ; appTable add old_table
      ; g_table := new_table
      ; g_limit := old_limit + 2
   end

and add (n as T {hash, tag, node}) =
   let
      val table = !g_table
      val index = Word.toInt hash mod Array.length table
      val bucket = Array.sub (table, index)
      val bkt_size = Array.length bucket

      fun go i =
         if i < bkt_size
            then
               case Weak.get (Array.sub (bucket, i)) of
                  NONE => Array.update (bucket, i, Weak.new n)
                | SOME m =>
                     if isTombstone m
                        then Array.update (bucket, i, Weak.new n)
                     else go (i + 1)
         else
            let
               val new_bkt_size = bkt_size + 3
               val new_bucket = Array.array (new_bkt_size, Weak.new (tombstone node))
            in
               Array.copy {di = 0, dst = new_bucket, src = bucket}
               ; Array.update (new_bucket, bkt_size, Weak.new n)
               ; Array.update (table, index, new_bucket)
               ; g_totsize := !g_totsize + 3
               ; if !g_totsize > !g_limit * Array.length table
                    then resize ()
                 else ()
            end
   in
      go 0
   end

fun hashcons n =
   let
      val h = Key.hash n
      val table = !g_table
      val index = Word.toInt h mod Array.length table
      val bucket = Array.sub (table, index)
      val sz = Array.length bucket

      fun go i =
         if i < sz
            then
               case Weak.get (Array.sub (bucket, i)) of
                  NONE => go (i + 1)
                | SOME (m as T {node, ...}) =>
                     if isTombstone m
                        then go (i + 1)
                     else if Key.equals (n, node)
                        then m
                     else go (i + 1)
         else
            let
               val m = T {hash = h, tag = nextTag (), node = n}
            in
               add m
               ; m
            end
   in
      go 0
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
