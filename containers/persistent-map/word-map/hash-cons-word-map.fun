
functor HashConsWordMapFn(S: HASH_CONS_WORD_MAP_STRUCTS) =
struct

open S

structure Tag :>
   sig
      type t

      val next: unit -> t
      val equals: t * t -> bool
   end =
   struct
      type t = word

      val counter = ref 0w0

      fun next () = !counter before counter := !counter + 0w1

      val equals: t * t -> bool = op =
   end

type key = word
type value = Value.t

datatype node =
   Nil
 | Tip of key * value
 | Bin of word * word * map * map

and map = Map of {hash: word, tag: Tag.t, node: node}

fun node (Map {node, ...}) = node
fun hash (Map {hash, ...}) = hash
fun tag (Map {tag, ...}) = tag

fun equals (Map {tag = t1, ...}, Map {tag = t2, ...}) = Tag.equals (t1, t2)

fun equalsNode (Nil, Nil) = true
  | equalsNode (Tip (k, x), Tip (k', x')) = k = k' andalso Value.equals (x, x')
  | equalsNode (Bin (p, m, l, r), Bin (p', m', l', r')) =
   p = p'
   andalso m = m'
   andalso equals (l, l')
   andalso equals (r, r')
  | equalsNode _ = false

(* Hash combining function from the Boost C++ library *)
fun hashCombine (seed, h) =
   Word.xorb (seed, h + 0wx9e3779b9 + Word.<< (seed, 0w6) + Word.>> (seed, 0w2))

fun hashNode Nil = 0w1
  | hashNode (Tip (k, x)) = hashCombine (k, Value.hash x)
  | hashNode (Bin (_, _, l, r)) = hashCombine (hash l, hash r)

structure HC =
   struct
      val tombstone_tag = Tag.next ()
      val tombstone = Weak.new (Map {hash = 0w0, tag = tombstone_tag, node = Nil})
      fun isTombstone m = Tag.equals (tag m, tombstone_tag)
      fun freeSlot w =
         case Weak.get w of
            NONE => true
          | SOME m => isTombstone m

      val empty_bucket: map Weak.t array = Array.fromList []

      val table = ref (Array.array (128, empty_bucket))

      val size = ref 0

      val limit = ref 3

      fun nextSize n = 3 * n div 2 + 3

      fun forEach tbl f =
         let
            fun goWeak w =
               case Weak.get w of
                  NONE => ()
                | SOME m => if isTombstone m then () else f m

            fun goBkt i bkt =
               if i < Array.length bkt
                  then (goWeak (Array.sub (bkt, i)); goBkt (i + 1) bkt)
               else ()
         in
            Array.app (goBkt 0) tbl
         end

      fun resize () =
         let
            val old_table = !table
            val old_limit = !limit
            val old_len = Array.length old_table
            val new_len = nextSize old_len
         in
            (* Re-initialize *)
            size := 0
            ; limit := old_limit + 100 (* Temporary *)
            ; table := Array.array (new_len, empty_bucket)
            (* Add all live elements from the old table *)
            ; forEach old_table add
            (* Increase the limit by 2 *)
            ; limit := old_limit + 2
         end

      and add m =
         let
            val index = Word.toInt (hash m) mod Array.length (!table)
            val bucket = Array.sub (!table, index)
            val bkt_size = Array.length bucket

            fun go i =
               if i < bkt_size
                  then
                     if freeSlot (Array.sub (bucket, i))
                        then Array.update (bucket, i, Weak.new m)
                     else go (i + 1)
               else
                  let
                     val new_size = bkt_size + 3
                     val new_bucket = Array.array (new_size, tombstone)
                  in
                     Array.copy {di = 0, dst = new_bucket, src = bucket}
                     ; Array.update (new_bucket, bkt_size, Weak.new m)
                     ; Array.update (!table, index, new_bucket)
                     ; size := !size + (new_size - bkt_size)
                     ; if !size > !limit * Array.length (!table)
                          then resize ()
                       else ()
                  end
         in
            go 0
         end

      fun hashcons n =
         let
            val hkey = hashNode n
            val index = Word.toInt hkey mod Array.length (!table)
            val bucket = Array.sub (!table, index)
            val size = Array.length bucket

            fun go i =
               if i >= size
                  then
                     let
                        val tag = Tag.next ()
                        val m = Map {hash = hkey, tag = tag, node = n}
                     in
                        add m
                        ; m
                     end
               else
                  let
                     val w = Array.sub (bucket, i)
                  in
                     case Weak.get w of
                        NONE => go (i + 1)
                      | SOME m =>
                           if equalsNode (n, node m)
                              then m
                           else go (i + 1)
                  end
         in
            go 0
         end
   end


(* val empty = Map (0w0, Nil) *)

(* fun tip (k, x) = *)
(*    let *)
(*    in *)
(*       () *)
(*    end *)

end

(* vim: set tw=0 ts=3 sw=3: *)
