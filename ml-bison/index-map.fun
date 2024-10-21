
structure IntHashSet = HashSetFn(
  struct
    type hash_key = int

    fun sameKey (x : int, y) = x = y
    val hashVal = Word.fromInt
  end)

signature INDEX_SET =
  sig
    type item
    type set


  end

functor IndexSetFn(Key : HASH_KEY) : INDEX_SET =
  struct
    structure Key = Key
    structure S = IntHashSet

    datatype set = Set of {
        indices : S.set,
        entries : (word * item) array ref,
        size : int ref
      }

    withtype item = Key.hash_key

    fun mkEmpty n = Set {
        indices = S.mkEmpty n,
        entries = ref (Array.fromList []),
        size = ref 0
      }

  end

