
structure Real16Impl =
struct
  structure W = Word
  structure W64 = Word64

  type real = word

  (* Word should always be >= 16 bits, but check anyways *)
  val () = if W.wordSize >= 16 then () else raise Fail "Word.wordSize < 16"

  (* Some useful constants *)
  val HALF_SIG_BITS = 0wxa
  val HALF_BITS = 0wx10
  val HALF_MASK = W.<< (0w1, HALF_BITS) - 0w1 (* 0wxffff *)
  val HALF_EXP_BITS = HALF_BITS - HALF_SIG_BITS - 0w1 (* 0w5 *)
  val HALF_INF_EXP = W.<< (0w1, HALF_EXP_BITS) - 0w1 (* 0wx1f *)
  val HALF_EXP_BIAS = W.>> (HALF_INF_EXP, 0w1) (* 0wxf *)
  val HALF_MIN_NORMAL = W.<< (0w1, HALF_SIG_BITS) (* 0wx0400 *)
  val HALF_INFINITY = W.<< (HALF_INF_EXP, HALF_SIG_BITS) (* 0wx7c00 *)
  val HALF_SIGN_MASK = W.<< (0w1, HALF_SIG_BITS + HALF_EXP_BITS) (* 0wx8000 *)
  val HALF_ABS_MASK = HALF_SIGN_MASK - 0w1 (* 0wx7fff *)
  val HALF_QNAN = W.<< (0w1, HALF_BITS - 0w1) (* 0wx0200 *)
  val HALF_NAN_CODE = HALF_QNAN - 0w1 (* 0wx01ff *)

  val FLOAT_SIG_BITS = 0w23
  val FLOAT_BITS = 0w32
  val FLOAT_MASK = W.<< (0w1, FLOAT_BITS) - 0w1
  val FLOAT_EXP_BITS = FLOAT_BITS - FLOAT_SIG_BITS - 0w1
  val FLOAT_INF_EXP = W.<< (0w1, FLOAT_EXP_BITS) - 0w1
  val FLOAT_EXP_BIAS = W.>> (FLOAT_INF_EXP, 0w1)
  val FLOAT_MIN_NORMAL = W.<< (0w1, FLOAT_SIG_BITS)
  val FLOAT_INFINITY = W.<< (FLOAT_INF_EXP, FLOAT_SIG_BITS)
  val FLOAT_SIGN_MASK = W.<< (0w1, FLOAT_SIG_BITS + FLOAT_EXP_BITS)
  val FLOAT_ABS_MASK = FLOAT_SIGN_MASK - 0w1
  val FLOAT_QNAN = W.<< (0w1, FLOAT_BITS - 0w1)
  val FLOAT_NAN_CODE = FLOAT_QNAN - 0w1

  val DOUBLE_SIG_BITS = 0w52
  val DOUBLE_BITS = 0w64
  val DOUBLE_EXP_BITS = DOUBLE_BITS - DOUBLE_SIG_BITS - 0w1
  val DOUBLE_INF_EXP = W64.<< (0w1, DOUBLE_EXP_BITS) - 0w1
  val DOUBLE_EXP_BIAS = W64.>> (DOUBLE_INF_EXP, 0w1)
  val DOUBLE_MIN_NORMAL = W64.<< (0w1, DOUBLE_SIG_BITS)
  val DOUBLE_INFINITY = W64.<< (DOUBLE_INF_EXP, DOUBLE_SIG_BITS)
  val DOUBLE_SIGN_MASK = W64.<< (0w1, DOUBLE_SIG_BITS + DOUBLE_EXP_BITS)
  val DOUBLE_ABS_MASK = DOUBLE_SIGN_MASK - 0w1
  val DOUBLE_QNAN = W64.<< (0w1, DOUBLE_BITS - 0w1)
  val DOUBLE_NAN_CODE = DOUBLE_QNAN - 0w1

  fun leadingZeros16 0w0 = 0w16
    | leadingZeros16 x =
      let
        val () = if W.andb (x, 0wxffff) = x then () else raise Domain
        val (n, x) = if W.andb (x, 0wxff00) = 0w0 then (0w8, W.<< (x, 0w8)) else (0w0, x)
        val (n, x) = if W.andb (x, 0wxf000) = 0w0 then (n + 0w4, W.<< (x, 0w4)) else (n, x)
        val (n, x) = if W.andb (x, 0wxc000) = 0w0 then (n + 0w2, W.<< (x, 0w2)) else (n, x)
      in
        if W.andb (x, 0wx8000) = 0w0 then n + 0w1 else n
      end

  fun leadingZeros32 0w0 = 0w32
    | leadingZeros32 x =
      let
        val () = if W.andb (x, 0wxffffffff) = x then () else raise Domain
        val (n, x) = if W.andb (x, 0wxffff0000) = 0w0 then (0w16, W.<< (x, 0w16)) else (0w0, x)
        val (n, x) = if W.andb (x, 0wxff000000) = 0w0 then (n + 0w8, W.<< (x, 0w8)) else (n, x)
        val (n, x) = if W.andb (x, 0wxf0000000) = 0w0 then (n + 0w4, W.<< (x, 0w4)) else (n, x)
        val (n, x) = if W.andb (x, 0wxc0000000) = 0w0 then (n + 0w2, W.<< (x, 0w2)) else (n, x)
      in
        if W.andb (x, 0wx80000000) = 0w0 then n + 0w1 else n
      end

  fun real16ToReal64 h =
    let
      val hAbs = W.andb (h, HALF_ABS_MASK)
      val hAbs' = W64.fromLarge (W.toLarge hAbs)
      val sign = W64.fromLarge (W.toLarge (W.andb (h, HALF_SIGN_MASK)))
      fun addSign w = W64.orb (w, W64.<< (sign, DOUBLE_BITS - HALF_BITS))
    in
      if W.andb (hAbs - HALF_MIN_NORMAL, HALF_MASK) < HALF_INFINITY - HALF_MIN_NORMAL
        then addSign (
            W64.<< (hAbs', DOUBLE_SIG_BITS - HALF_SIG_BITS)
          + W64.<< (DOUBLE_EXP_BIAS - W.toLarge HALF_EXP_BIAS, DOUBLE_SIG_BITS))
      else if hAbs >= HALF_INFINITY
        then
          let
            val r = W64.<< (DOUBLE_INF_EXP, DOUBLE_SIG_BITS)
            val r = W64.orb (r, W64.<< (W.toLarge (W.andb (hAbs, HALF_QNAN)), DOUBLE_SIG_BITS - HALF_SIG_BITS))
            val r = W64.orb (r, W64.<< (W.toLarge (W.andb (hAbs, HALF_NAN_CODE)), DOUBLE_SIG_BITS - HALF_SIG_BITS))
          in
            addSign r
          end
      else if hAbs <> 0w0
        then
          let
            val scale = leadingZeros16 hAbs - leadingZeros16 HALF_MIN_NORMAL
            val resultExp = DOUBLE_EXP_BIAS - W.toLarge HALF_EXP_BIAS - W.toLarge scale + 0w1

            val () = if 0w1 <= scale andalso scale <= 0w10 then () else raise Fail ("scale out of range " ^ W.toString scale)
            val r = W64.<< (hAbs', DOUBLE_SIG_BITS - HALF_SIG_BITS + scale)
            val r = W64.xorb (r, DOUBLE_MIN_NORMAL)
            val r = W64.orb (r, W64.<< (resultExp, DOUBLE_SIG_BITS))
          in
            addSign r
          end
      else addSign 0w0
    end

  fun toReal64 h = RealConversions.word64ToReal64 (real16ToReal64 h)
end

structure PackReal16Big : PACK_REAL =
struct
  type real = Real16Impl.real

  val bytesPerElem = 2
  val isBigEndian = true

  fun toBytes r = raise Fail "NYI"
  fun fromBytes bs = raise Fail "NYI"
  fun subVec (bs, i) = raise Fail "NYI"
  fun subArr (bs, i) = raise Fail "NYI"
  fun update (bs, i, r) = raise Fail "NYI"
end

structure Real16Test =
struct
  fun run instream =
    let
      val buf = BinIO.inputN (instream, 16)
      val () = if Word8Vector.length buf < 16 then raise Fail "End of stream" else ()
      val word16 =
        Word.fromLarge (Word8.toLarge (Word8Vector.sub (buf, 0)))
        + Word.<< (Word.fromLarge (Word8.toLarge (Word8Vector.sub (buf, 1))), 0w8)
      val expected = PackReal64Little.subVec (buf, 1)
      val actual = Real16Impl.toReal64 word16
    in
      if (Real64.isNan expected andalso Real64.isNan actual)
          orelse RealConversions.real64ToWord64 expected = RealConversions.real64ToWord64 actual
        then ()
      else
        print ("word16 = 0wx" ^ Word.toString word16
          ^ ", expected = " ^ Real64.toString expected
          ^ " (0wx" ^ Word64.toString (RealConversions.real64ToWord64 expected) ^ ") "
          ^ ", actual = " ^ Real64.toString actual
          ^ " (0wx" ^ Word64.toString (RealConversions.real64ToWord64 actual) ^ ") "
          ^ "\n");
      run instream
    end

  fun test () =
    let
      val s = BinIO.openIn "test/out.bin"
    in
      (run s handle Fail msg => msg)
      before
      (BinIO.closeIn s; print "done\n")
    end

  val _ = test ()
end

