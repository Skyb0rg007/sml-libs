(* vim: set tw=0 ts=3 sw=3: *)

functor HAMTFn(Key: HASH_KEY) =
struct
   open Util

   structure Key = Key
   structure SV = SparseVector
   structure VM = VectorMapFn(
      struct
         type eq_key = Key.hash_key
         val sameKey = Key.sameKey
      end)

   datatype 'a map =
      EMPTY
    | NODE of 'a map SV.t
    | LEAF of Word32.word * Key.hash_key * 'a
    | COLLISION of Word32.word * 'a VM.map

   fun hashKey k =
      Word32.fromLarge (Word.toLarge (Word.andb (Key.hashVal k, 0wxffffffff)))

   val bitsPerSubkey = 0w5

   fun index (h, s) = Word32.toInt (Word32.andb (Word32.>> (h, s), 0wx1f))

   (**)

   fun find (m, k) =
      let
         val h = hashKey k
         fun go (_, EMPTY) = NONE
           | go (_, LEAF (h', k', v)) =
            if h = h' andalso Key.sameKey (k, k')
               then SOME v
            else NONE
           | go (_, COLLISION (h', kvs)) =
            if h = h'
               then VM.find (kvs, k)
            else NONE
           | go (s, NODE sv) =
            case SV.find (sv, index (h, s)) of
               NONE => NONE
             | SOME m => go (s + bitsPerSubkey, m)
      in
         go (0w0, m)
      end

   fun insertLookupWithi f (m, k, v) =
      let
         val h = hashKey k
         fun go (_, EMPTY) = (LEAF (h, k, v), NONE)
           | go (s, LEAF (h', k', v')) =
            if h = h'
               then
                  if Key.sameKey (k, k')
                     then (LEAF (h, k', f (k', v', v)), SOME v')
                  else (COLLISION (h, VM.fromDistinctList [(k, v), (k', v')]), NONE)
            else
               let
                  val leaf1 = LEAF (h, k, v)
                  val leaf2 = LEAF (h', k', v')
                  fun loop s =
                     let
                        val i = index (h, s)
                        val i' = index (h', s)
                     in
                        if i = i'
                           then NODE (SV.singleton (i, loop (s + bitsPerSubkey)))
                        else
                           NODE (SV.fromList [(i, leaf1), (i', leaf2)])
                     end
               in
                  (loop s, NONE)
               end
           | go (s, m as COLLISION (h', kvs)) =
            if h = h'
               then
                  case VM.insertLookupWithi f (kvs, k, v) of
                     (kvs', ov') => (COLLISION (h, kvs'), ov')
            else
               go (s, NODE (SV.singleton (index (h', s), m)))
           | go (s, NODE sv) =
            let
               val i = index (h, s)
               val (submap, ov') =
                  case SV.find (sv, i) of
                     NONE => (LEAF (h, k, v), NONE)
                   | SOME m' => go (s + bitsPerSubkey, m')
            in
               (NODE (SV.insert (sv, i, submap)), ov')
            end
      in
         go (0w0, m)
      end
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
