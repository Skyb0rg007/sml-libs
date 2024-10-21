
val arr = Word8Array.array (8, 0w0)

val r = 3.14
val () = PackReal64Little.update (arr, 0, r)
val w = PackWord64Little.subArr (arr, 0)
