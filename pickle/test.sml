
structure Test =
struct

fun wordToReal w =
   let
      val arr = Word8Array.array (PackReal64Little.bytesPerElem, 0w0)
   in
      PackWord64Big.update (arr, 0, w);
      PackReal64Little.subArr (arr, 0)
   end
(* val wordToReal = MLton.Real64.castFromWord *)

fun jcsTest1 () =
   let
      val u = Byte.bytesToString o Word8Vector.fromList
      val s = Encoding.string
      val input =
         Encoding.canonObject
         [(u [0wxE2, 0wx82, 0wxAC] (* U+20AC *), s "Euro Sign"),
          ("\r" (* U+000D *), s "Carriage Return"),
          (u [0wxEF, 0wxAC, 0wxB3] (* U+FB33 *), s "Hebrew Letter Dalet with Dagesh"),
          ("1" (* U+0031 *), s "One"),
          (u [0wxF0, 0wx9F, 0wx98, 0wx80] (* U+01F600 (0xD83D 0xDE00) *), s "Emoji: Grinning Face"),
          (u [0wxC2, 0wx80] (* U+0080 *), s "Control"),
          (u [0wxC3, 0wxB6] (* U+00F6 *), s "Latin Small Letter O With Diaeresis")]
      val expected =
         Encoding.object
         [("\r", s "Carriage Return"),
          ("1", s "One"),
          (u [0wxC2, 0wx80], s "Control"),
          (u [0wxC3, 0wxB6], s "Latin Small Letter O With Diaeresis"),
          (u [0wxE2, 0wx82, 0wxAC], s "Euro Sign"),
          (u [0wxF0, 0wx9F, 0wx98, 0wx80], s "Emoji: Grinning Face"),
          (u [0wxEF, 0wxAC, 0wxB3], s "Hebrew Letter Dalet with Dagesh")]
   in
      if Encoding.toString input = Encoding.toString expected
         then print "OK\n"
      else raise Fail "jcsTest1 - canonicalization failed"
   end

fun jcsTest2 () =
   let
      open Encoding
      val input =
         canonObject
         [("literals", array [null, true_, false_]),
          ("numbers", array [real 333333333.3333333, real 1.0e30, real 4.5, real 0.002, real 1.0e~27]),
          ("string", string "€$\^O\nA'B\"\\\\\"/")]
      val expected = Byte.bytesToString (Word8Vector.fromList
         [0wx7b, 0wx22, 0wx6c, 0wx69, 0wx74, 0wx65, 0wx72, 0wx61, 0wx6c, 0wx73,
          0wx22, 0wx3a, 0wx5b, 0wx6e, 0wx75, 0wx6c, 0wx6c, 0wx2c, 0wx74, 0wx72,
          0wx75, 0wx65, 0wx2c, 0wx66, 0wx61, 0wx6c, 0wx73, 0wx65, 0wx5d, 0wx2c,
          0wx22, 0wx6e, 0wx75, 0wx6d, 0wx62, 0wx65, 0wx72, 0wx73, 0wx22, 0wx3a,
          0wx5b, 0wx33, 0wx33, 0wx33, 0wx33, 0wx33, 0wx33, 0wx33, 0wx33, 0wx33,
          0wx2e, 0wx33, 0wx33, 0wx33, 0wx33, 0wx33, 0wx33, 0wx33, 0wx2c, 0wx31,
          0wx65, 0wx2b, 0wx33, 0wx30, 0wx2c, 0wx34, 0wx2e, 0wx35, 0wx2c, 0wx30,
          0wx2e, 0wx30, 0wx30, 0wx32, 0wx2c, 0wx31, 0wx65, 0wx2d, 0wx32, 0wx37,
          0wx5d, 0wx2c, 0wx22, 0wx73, 0wx74, 0wx72, 0wx69, 0wx6e, 0wx67, 0wx22,
          0wx3a, 0wx22, 0wxe2, 0wx82, 0wxac, 0wx24, 0wx5c, 0wx75, 0wx30, 0wx30,
          0wx30, 0wx66, 0wx5c, 0wx6e, 0wx41, 0wx27, 0wx42, 0wx5c, 0wx22, 0wx5c,
          0wx5c, 0wx5c, 0wx5c, 0wx5c, 0wx22, 0wx2f, 0wx22, 0wx7d])
   in
      if Encoding.toString input = expected
         then print "OK\n"
      else (print (Encoding.toString input ^ "\n");
            raise Fail "jcsTest2 - canonicalization failed")
   end

fun jcsTest3 () =
   let
      (* https://datatracker.ietf.org/doc/html/rfc8785#section-appendix.b *)
      val tests : (LargeWord.word * string * string) list =
         [(0wx0000000000000000, "0", "Zero"),
          (0wx8000000000000000, "0", "Minus zero"),
          (0wx0000000000000001, "5e-324", "Min pos number"),
          (0wx8000000000000001, "-5e-324", "Min neg number"),
          (0wx7fefffffffffffff, "1.7976931348623157e+308", "Max pos number"),
          (0wxffefffffffffffff, "-1.7976931348623157e+308", "Max neg number"),
          (0wx4340000000000000, "9007199254740992", "Max pos int"),
          (0wxc340000000000000, "-9007199254740992", "Max neg int"),
          (0wx4430000000000000, "295147905179352830000", "~2^68"),
          (0wx44b52d02c7e14af5, "9.999999999999997e+22", ""),
          (0wx44b52d02c7e14af6, "1e+23", ""),
          (0wx44b52d02c7e14af7, "1.0000000000000001e+23", ""),
          (0wx444b1ae4d6e2ef4e, "999999999999999700000", ""),
          (0wx444b1ae4d6e2ef4f, "999999999999999900000", ""),
          (0wx444b1ae4d6e2ef50, "1e+21", ""),
          (0wx3eb0c6f7a0b5ed8c, "9.999999999999997e-7", ""),
          (0wx3eb0c6f7a0b5ed8d, "0.000001", ""),
          (0wx41b3de4355555553, "333333333.3333332", ""),
          (0wx41b3de4355555554, "333333333.33333325", ""),
          (0wx41b3de4355555555, "333333333.3333333", ""),
          (0wx41b3de4355555556, "333333333.3333334", ""),
          (0wx41b3de4355555557, "333333333.33333343", ""),
          (0wxbecbf647612f3696, "-0.0000033333333333333333", ""),
          (0wx43143ff3c1cb0959, "1424953923781206.2", "Round to even")]
   in
      (* `real` matches the examples *)
      List.app
         (fn (w, expected, comment) =>
            let
               val r = wordToReal w

               val actual = Encoding.toString (Encoding.real r)
            in
               if actual = expected
                  then ()
               else
                   raise Fail "jcsTest3 - canonicalization failure"
            end)
         tests;
      (* `real` errors out on NaN and Infinity *)
      List.app
         (fn r =>
            if (Encoding.real r; false) handle _ => true
               then ()
            else raise Fail "jcsTest3 - failed to error on NaN or Infinity")
         [0.0/0.0, Real.posInf, Real.negInf];
      print "OK\n"
   end


fun jcsTest4 () =
   let
      val input =
         Encoding.array
         [Encoding.real 56.0,
          Encoding.canonObject
          [("d", Encoding.true_),
           ("10", Encoding.null),
           ("1", Encoding.array [])]]
      val expected =
         "[56,{\"1\":[],\"10\":null,\"d\":true}]"
   in
      if Encoding.toString input = expected
         then print "OK\n"
      else raise Fail "jcsTest3 - canonicalization failed"
   end

fun jcsTest4 () =
   let
      val input =
         Encoding.canonObject
         [("peach", Encoding.string "This sorting order"),
          ("péché", Encoding.string "is wrong according to French"),
          ("pêche", Encoding.string "but canonicalization MUST"),
          ("sin", Encoding.string "ignore locale")]
      val expected =
         "{\"peach\":\"This sorting order\",\"péché\":\"is wrong according to French\",\"pêche\":\"but canonicalization MUST\",\"sin\":\"ignore locale\"}"
   in
      if Encoding.toString input = expected
         then print "OK\n"
      else raise Fail "jcsTest5 - canonicalization failed"
   end

fun jcsTest5 () =
   let
      val input =
         Encoding.canonObject
         [("1", Encoding.canonObject
                [("f", Encoding.canonObject [("f", Encoding.string "hi"), ("F", Encoding.real 5.0)]),
                 ("\n", Encoding.real 56.0)]),
          ("10", Encoding.canonObject []),
          ("", Encoding.string "empty"),
          ("a", Encoding.canonObject []),
          ("111", Encoding.array [Encoding.canonObject [("e", Encoding.string "yes"), ("E", Encoding.string "no")]]),
          ("A", Encoding.canonObject [])]
      val expected =
         "{\"\":\"empty\",\"1\":{\"\\n\":56,\"f\":{\"F\":5,\"f\":\"hi\"}},\"10\":{},\"111\":[{\"E\":\"no\",\"e\":\"yes\"}],\"A\":{},\"a\":{}}"
   in
      if Encoding.toString input = expected
         then print "OK\n"
      else raise Fail "jcsTest5 - canonicalization failed"
   end

fun testAll () =
   (jcsTest1 ();
    jcsTest2 ();
    jcsTest3 ();
    jcsTest4 ();
    jcsTest5 ())

val () = testAll ()

end

(* vim: set ts=3 sw=3: *)
