
structure RealConversions =
struct
  val real64ToWord64 = Unsafe.realToBits

  (* Test for bug on SML/NJ 110.99.4 and below
   * `PackWord64Little.update` and `PackWord64Big.update` are swapped
   *)
  local
    val arr = Word8Array.array (8, 0w0)
    val w : Word64.word = 0wxdeadbeefcafebabe
    val () = PackWord64Little.update (arr, 0, w)
    val wL = PackWord64Little.subArr (arr, 0)
    val () = Word8Array.modify (fn _ => 0w0) arr
    val () = PackWord64Big.update (arr, 0, w)
    val wB = PackWord64Little.subArr (arr, 0)
  in
    val packWord64LittleUpdate =
      if w = wL
        then PackWord64Little.update
      else if w = wB
        then PackWord64Big.update
      else raise Fail "Invalid PackWord64Little.update"
  end

  fun word64ToReal64 w =
    let
      val arr = Word8Array.array (8, 0w0)
    in
      packWord64LittleUpdate (arr, 0, w);
      PackReal64Little.subArr (arr, 0)
    end
end
