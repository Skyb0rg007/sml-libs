
structure SExp =
struct
   structure Num =
   struct
      datatype t =
         FIX of int
       | BIG of IntInf.int
       | FLO of real
       | RAT of t * t
       | CPX of t * t

      fun toString (FIX n) =
         if n < 0
            then "-" ^ Int.toString (~n)
         else Int.toString n
        | toString (BIG n) =
         if n < 0
            then "-" ^ IntInf.toString (~n)
         else IntInf.toString n
        | toString (FLO n) =
         if Real.isNan n
            then "+nan.0"
         else
            (case (Real.isFinite n, n < 0.0) of
                (true, true) => "-" ^ Real.toString (~n)
              | (true, false) => Real.toString n
              | (false, true) => "-inf.0"
              | (false, false) => "+inf.0")
        | toString (RAT (n, d)) = toString n ^ "/" ^ toString d
        | toString (CPX (a, b)) =
         let
            val bstr = toString b
         in
            if String.sub (bstr, 0) = #"-"
               then toString a ^ bstr ^ "i"
            else toString a ^ "+" ^ bstr ^ "i"
         end

      val bignum =
         case (Int.minInt, Int.maxInt) of
            (SOME _, NONE) => raise Fail "Int has minimum value but no maximum"
          | (NONE, SOME _) => raise Fail "Int has maximum value but no minimum"
          | (SOME min, SOME max) =>
               let
                  val min = IntInf.fromInt min
                  val max = IntInf.fromInt max
                  fun go n =
                     if min <= n andalso n <= max
                        then FIX (IntInf.toInt n)
                     else BIG n
               in
                  go
               end
          | (NONE, NONE) => fn n => FIX (IntInf.toInt n)

      fun exact (FIX _) = true
        | exact (BIG _) = true
        | exact (FLO _) = false
        | exact (RAT _) = true
        | exact (CPX (a, b)) = exact a andalso exact b

      fun remainder (FIX a, FIX b) = FIX (Int.rem (a, b))
        | remainder (BIG a, BIG b) = BIG (IntInf.rem (a, b))
        | remainder (FIX a, BIG b) = BIG (IntInf.rem (IntInf.fromInt a, b))
        | remainder (BIG a, FIX b) = BIG (IntInf.rem (a, IntInf.fromInt b))
        | remainder _ = raise Fail "Invalid input to remainder"

      fun ratnum (n, d) =
         if not (exact n andalso exact d)
            then raise Fail "Inexact rational"
         else
            case (n, d) of
               (_, FIX 0) => raise Div
             | (FIX 0, _) => FIX 0
             | _ => raise Fail "NYI"
   end

   datatype sexp =
      (* Atoms *)
      SYMBOL of string
    | STRING of string
    | NUMBER of Num.t
    | BYTEVECTOR of Word8Vector.vector
    | NIL
    | TRUE
    | FALSE
    (* Compound *)
    | CONS of sexp * sexp
    | VECTOR of sexp vector

   fun list2 (a, b) = CONS (a, CONS (b, NIL))

   fun isSeparator c =
      case c of
         #"\t" => true
       | #"\n" => true
       | #"\v" => true
       | #"\f" => true
       | #"\r" => true
       | #" " => true
       | #"\"" => true
       | #"'" => true
       | #"(" => true
       | #")" => true
       | #"," => true
       | #";" => true
       | #"[" => true
       | #"]" => true
       | #"{" => true
       | #"}" => true
       | _ => false

   fun hexDigit n =
      if 0 <= n andalso n <= 9
         then Char.chr (Char.ord #"0" + n)
      else if 10 <= n andalso n <= 15
         then Char.chr (Char.ord #"a" + n)
      else raise Fail "Not a hex digit!"

   fun toString (SYMBOL sym) =
      let
         fun weirdChar c =
            c <= #" "
            orelse c = #"\\"
            orelse c = #"|"
            orelse c = #"#"
            orelse isSeparator c

         val bar =
            String.size sym = 0
            orelse sym = "."
            orelse String.isPrefix "+nan" sym
            orelse String.isPrefix "-nan" sym
            orelse String.isPrefix "+inf" sym
            orelse String.isPrefix "-inf" sym
            orelse String.isPrefix "+." sym
            orelse String.isPrefix "-." sym
            orelse CharVector.exists weirdChar sym

         fun escape #"\\" = "\\\\"
           | escape #"|" = "\\|"
           | escape c = String.str c
      in
         if bar
            then "|" ^ String.translate escape sym ^ "|"
         else String.translate escape sym
      end
     | toString (STRING str) =
      let
         fun escape #"\\" = "\\\\"
           | escape #"\"" = "\\\""
           | escape #"\a" = "\\a"
           | escape #"\b" = "\\b"
           | escape #"\n" = "\\n"
           | escape #"\r" = "\\r"
           | escape #"\t" = "\\t"
           | escape c =
            if c < #" "
               then
                  let
                     val w = Word.fromInt (Char.ord c)
                     val d1 = Word.toInt (Word.>> (w, 0w4))
                     val d2 = Word.toInt (Word.andb (w, 0wxf))
                  in
                     String.implode [#"\\", #"x", hexDigit d1, hexDigit d2, #";"]
                  end
            else String.str c
      in
         "\"" ^ String.translate escape str ^ "\""
      end
     | toString (NUMBER n) = Num.toString n
     | toString (BYTEVECTOR v) =
      let
         fun go (w, "") = Word8.fmt StringCvt.DEC w
           | go (w, s) = Word8.fmt StringCvt.DEC w ^ " " ^ s
      in
         "#u8(" ^ Word8Vector.foldr go "" v ^ ")"
      end
     | toString (VECTOR v) =
      let
         fun go (x, "") = toString x
           | go (x, s) = toString x ^ " " ^ s
      in
         "#(" ^ Vector.foldr go "" v ^ ")"
      end
     | toString TRUE = "#t"
     | toString FALSE = "#f"
     | toString NIL = "()"
     | toString (CONS (car, cdr)) =
      let
         fun go (CONS (car, cdr), acc) = go (cdr, toString car :: " " :: acc)
           | go (NIL, acc) = acc
           | go (s, acc) = toString s :: " . " :: acc
      in
         "(" ^ toString car ^ String.concat (List.rev (go (cdr, []))) ^ ")"
      end


   exception ParseError of string

   datatype raw =
      CLOSE
    | DOT
    | SEXP of sexp

   fun rawSexp (SEXP sexp) = sexp
     | rawSexp CLOSE = raise ParseError "Invalid ')'"
     | rawSexp DOT = raise ParseError "Invalid '.'"

   fun letter getc s =
      case getc s of
         NONE => NONE
       | SOME (c, s') =>
            if #"a" <= c andalso c <= #"z"
               then SOME (c, s')
            else if #"A" <= c andalso c <= #"Z"
               then SOME (c, s')
            else NONE

   fun specialInitial getc s =
      case getc s of
         SOME (#"!", s) => SOME (#"!", s)
       | SOME (#"$", s) => SOME (#"$", s)
       | SOME (#"%", s) => SOME (#"%", s)
       | SOME (#"&", s) => SOME (#"&", s)
       | SOME (#"*", s) => SOME (#"*", s)
       | SOME (#"/", s) => SOME (#"/", s)
       | SOME (#":", s) => SOME (#":", s)
       | SOME (#"<", s) => SOME (#"<", s)
       | SOME (#"=", s) => SOME (#"=", s)
       | SOME (#">", s) => SOME (#">", s)
       | SOME (#"?", s) => SOME (#"?", s)
       | SOME (#"^", s) => SOME (#"^", s)
       | SOME (#"_", s) => SOME (#"_", s)
       | SOME (#"~", s) => SOME (#"~", s)
       | _ => NONE

   fun digit getc s =
      case getc s of
         NONE => NONE
       | SOME (c, s) =>
            if #"0" <= c andalso c <= #"9"
               then SOME (Char.ord c - Char.ord #"0", s)
            else NONE

   fun hexDigit getc s =
      case getc s of
         NONE => NONE
       | SOME (c, s) =>
            if #"0" <= c andalso c <= #"9"
               then SOME (Char.ord c - Char.ord #"0", s)
            else if #"a" <= c andalso c <= #"f"
               then SOME (Char.ord c - Char.ord #"a" + 10, s)
            else if #"A" <= c andalso c <= #"F"
               then SOME (Char.ord c - Char.ord #"A" + 10, s)
            else NONE

   fun hexScalarValue getc s =
      case hexDigit getc s of
         NONE => NONE
       | SOME (d, s) =>
            let
               fun go (n, s) =
                  case hexDigit getc s of
                     NONE => SOME (n, s)
                   | SOME (d, s) => go (16 * n + IntInf.fromInt d, s)
            in
               go (IntInf.fromInt d, s)
            end

   fun readString sentinel getc =
      let
         fun go acc s =
            case getc s of
               NONE => NONE
             | SOME (#"\\", s) =>
                  (case getc s of
                      NONE => NONE
                    | SOME (#"a", s) => go (#"\a" :: acc) s
                    | SOME (#"b", s) => go (#"\b" :: acc) s
                    | SOME (#"n", s) => go (#"\n" :: acc) s
                    | SOME (#"r", s) => go (#"\r" :: acc) s
                    | SOME (#"t", s) => go (#"\t" :: acc) s
                    | SOME (#"x", s) =>
                         let
                            fun goHex (s, acc) =
                               case getc s of
                                  NONE => raise Fail "Unexpected eof"
                                | SOME (#";", s) => (acc, s)
                                | SOME (c, s') =>
                                     if #"0" <= c andalso c <= #"9"
                                        then goHex (s', Char.ord c - Char.ord #"0" + 16 * acc)
                                     else if #"a" <= c andalso c <= #"f"
                                        then goHex (s', Char.ord c - Char.ord #"a" + 10 + 16 * acc)
                                     else if #"A" <= c andalso c <= #"F"
                                        then goHex (s', Char.ord c - Char.ord #"F" + 10 + 16 * acc)
                                     else raise Fail "Unexpected character in hex escape"
                            val (n, s) = goHex (s, 0)
                            val len =
                               if n < 0x80
                                  then 1
                               else if n < 0x800
                                  then 2
                               else if n < 0x10000
                                  then 3
                               else 4
                         in
                            go (Char.chr n :: acc) s
                         end
                    | SOME (c, s) =>
                         if Char.isSpace c
                            then
                               raise Fail "TODO: string newline escape"
                         else raise Fail "Unexpected character in string")
             | SOME (c, s) =>
                  if c = sentinel
                     then SOME (String.implodeRev acc, s)
                  else go (c :: acc) s
      in
         go []
      end

   fun readRaw (getc : (char, 's) StringCvt.reader) (s: 's): (sexp * 's) option =
      case getc s of
         NONE => NONE
       | SOME (#";", s) =>
            let
               fun go s =
                  case getc s of
                     SOME (#"\n", s) => readRaw getc s
                   | SOME (_, s) => go s
                   | NONE => NONE
            in
               go s
            end
       | SOME (#"\n", s) => readRaw getc s
       | SOME (#" ", s) => readRaw getc s
       | SOME (#"\t", s) => readRaw getc s
       | SOME (#"\f", s) => readRaw getc s
       | SOME (#"\r", s) => readRaw getc s
       | SOME (#"'", s) =>
            (case readRaw getc s of
                NONE => NONE
              | SOME (sexp, s) => SOME (list2 (SYMBOL "quote", sexp), s))
       | SOME (#"`", s) =>
            (case readRaw getc s of
                NONE => NONE
              | SOME (sexp, s) => SOME (list2 (SYMBOL "quasiquote", sexp), s))
       | SOME (#",", s) =>
            (case getc s of
                SOME (#"@", s) =>
                   (case readRaw getc s of
                       NONE => NONE
                     | SOME (sexp, s) => SOME (list2 (SYMBOL "unquote-splicing", sexp), s))
              | _ =>
                   case readRaw getc s of
                      NONE => NONE
                    | SOME (sexp, s) => SOME (list2 (SYMBOL "unquote", sexp), s))
       | SOME (#"\"", s) =>
            Option.map (fn (str, s) => (STRING str, s)) (readString #"\"" getc s)
       | SOME (#"(", s) => raise Fail "TODO: openParen"
       | SOME (#"#", s) => raise Fail "TODO: hash"
       | SOME (#".", s) => raise Fail "TODO: dot"
       | SOME (#")", s) => raise Fail "TODO: closeParen"
       | SOME (#"|", s) =>
            Option.map (fn (str, s) => (SYMBOL str, s)) (readString #"|" getc s)
       | SOME (#"+", s) => raise Fail "TODO: readNum"
       | SOME (#"-", s) => raise Fail "TODO: readNum"
       | SOME (c, s) =>
            if #"0" <= c andalso c <= #"9"
               then raise Fail "TODO: readNum"
            else raise Fail "TODO: readSym"

   structure P = ParserComb

   fun intralineWhitespace x =
      P.or' [P.char #" ", P.char #"\t"] x

   fun rawWhitespace x =
      P.or (intralineWhitespace, P.char #"\n") x

   fun delimiter x =
      P.or' [rawWhitespace, P.char #"|", P.char #"("] x

   fun boolean x =
      P.or' [P.string "#true", P.string "#false", P.string "#t", P.string "#f"] x
end

(* vim: set ts=3 sw=3 tw=0: *)
