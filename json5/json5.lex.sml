structure JSON5Lexer  = struct

    datatype yystart_state = 
DQ | SQ | INITIAL
    local

    structure UserDeclarations = 
      struct

 
   structure Tokens =
      struct
         datatype t =
            STRING of string
          | INT of IntInf.int
          | FLOAT of real
          | COMMA
          | COLON
          | LBRACE
          | RBRACE
          | LBRACK
          | RBRACK
          | NULL
          | TRUE
          | FALSE
          | ERROR of string
          | EOF
      end

   type lex_result = Tokens.t
   fun eof () = Tokens.EOF

   structure T = Tokens
   structure CB = CharBuffer

   fun int radix s =
      T.INT (Option.valOf (StringCvt.scanString (IntInf.scan radix) s))

   fun float s =
      T.FLOAT (Option.valOf (StringCvt.scanString Real.scan s))

   fun hex substr =
      #1 (Option.valOf (Word.scan StringCvt.HEX Substring.getc substr))

      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
Vector.fromList []
    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yyarg as  buf: CB.buf)(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case ULexBuffer.getu strm
                of (SOME (0w10, s')) => 
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;   skip() )
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LBRACE )
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;   T.RBRACE )
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LBRACK )
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;   T.RBRACK )
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;   T.COMMA )
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;   T.COLON )
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;   T.NULL )
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;   T.TRUE )
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;   T.FALSE )
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
        YYBEGIN DQ; CB.clear buf; continue () )
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
        YYBEGIN SQ; CB.clear buf; continue () )
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         
    T.ERROR (String.concat
        ["unescaped line terminator '", String.toString yytext, "'"])

      end
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
        YYBEGIN INITIAL; T.STRING (CB.contents buf) before CB.clear buf )
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
        YYBEGIN INITIAL; T.STRING (CB.contents buf) before CB.clear buf )
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
        CB.add1 (buf, #"'"); continue () )
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
        CB.add1 (buf, #"\""); continue () )
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
        CB.add1 (buf, #"\\"); continue () )
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
        CB.add1 (buf, #"\b"); continue () )
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
        CB.add1 (buf, #"\f"); continue () )
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
        CB.add1 (buf, #"\n"); continue () )
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
        CB.add1 (buf, #"\r"); continue () )
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
        CB.add1 (buf, #"\t"); continue () )
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
        CB.add1 (buf, #"\v"); continue () )
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
        T.ERROR "unrecognized escape sequence" )
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
        CB.add1 (buf, #"\000"); continue () )
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;
         
    CB.addVec (buf, UTF8.encode (hex (Substring.triml 2 yysubstr)));
    continue ()

      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;
         
    CB.addVec (buf, UTF8.encode (hex (Substring.triml 2 yysubstr)));
    continue ()

      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;
          CB.add1 (buf, Substring.sub (yysubstr, 1)); continue () 
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;   CB.addSlice (buf, yysubstr); continue () 
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;
         
print ("adding slice " ^ yytext ^ "\n");
print ("adding slice " ^ Substring.string yysubstr ^ "\n");
CB.addSlice (buf, yysubstr); continue () 
      end
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;   continue () )
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx75
              then yyQ48(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ47(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ51(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx75
              then yyQ50(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ56(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ55(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ54(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ53(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wxD
              then if inp = 0wx9
                  then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= 0wxA
                  then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx20
              then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then if ULexBuffer.eof(!(yystrm))
                  then let
                    val yycolno = ref(yygetcolNo(!(yystrm)))
                    val yylineno = ref(yygetlineNo(!(yystrm)))
                    in
                      (case (!(yyss))
                       of _ => (UserDeclarations.eof())
                      (* end case *))
                    end
                  else yystuck(lastMatch)
            else if inp < 0wx3B
              then if inp = 0wx22
                  then yyQ36(strm', lastMatch)
                else if inp < 0wx22
                  then if inp = 0wxD
                      then yyQ35(strm', lastMatch)
                    else if inp < 0wxD
                      then if inp = 0wx9
                          then yyQ35(strm', lastMatch)
                        else if inp < 0wx9
                          then if ULexBuffer.eof(!(yystrm))
                              then let
                                val yycolno = ref(yygetcolNo(!(yystrm)))
                                val yylineno = ref(yygetlineNo(!(yystrm)))
                                in
                                  (case (!(yyss))
                                   of _ => (UserDeclarations.eof())
                                  (* end case *))
                                end
                              else yystuck(lastMatch)
                        else if inp <= 0wxA
                          then yyQ35(strm', lastMatch)
                        else if ULexBuffer.eof(!(yystrm))
                          then let
                            val yycolno = ref(yygetcolNo(!(yystrm)))
                            val yylineno = ref(yygetlineNo(!(yystrm)))
                            in
                              (case (!(yyss))
                               of _ => (UserDeclarations.eof())
                              (* end case *))
                            end
                          else yystuck(lastMatch)
                    else if inp = 0wx20
                      then yyQ35(strm', lastMatch)
                    else if ULexBuffer.eof(!(yystrm))
                      then let
                        val yycolno = ref(yygetcolNo(!(yystrm)))
                        val yylineno = ref(yygetlineNo(!(yystrm)))
                        in
                          (case (!(yyss))
                           of _ => (UserDeclarations.eof())
                          (* end case *))
                        end
                      else yystuck(lastMatch)
                else if inp = 0wx2C
                  then yyQ38(strm', lastMatch)
                else if inp < 0wx2C
                  then if inp = 0wx27
                      then yyQ37(strm', lastMatch)
                    else if ULexBuffer.eof(!(yystrm))
                      then let
                        val yycolno = ref(yygetcolNo(!(yystrm)))
                        val yylineno = ref(yygetlineNo(!(yystrm)))
                        in
                          (case (!(yyss))
                           of _ => (UserDeclarations.eof())
                          (* end case *))
                        end
                      else yystuck(lastMatch)
                else if inp = 0wx3A
                  then yyQ39(strm', lastMatch)
                else if ULexBuffer.eof(!(yystrm))
                  then let
                    val yycolno = ref(yygetcolNo(!(yystrm)))
                    val yylineno = ref(yygetlineNo(!(yystrm)))
                    in
                      (case (!(yyss))
                       of _ => (UserDeclarations.eof())
                      (* end case *))
                    end
                  else yystuck(lastMatch)
            else if inp = 0wx6E
              then yyQ43(strm', lastMatch)
            else if inp < 0wx6E
              then if inp = 0wx5D
                  then yyQ41(strm', lastMatch)
                else if inp < 0wx5D
                  then if inp = 0wx5B
                      then yyQ40(strm', lastMatch)
                    else if ULexBuffer.eof(!(yystrm))
                      then let
                        val yycolno = ref(yygetcolNo(!(yystrm)))
                        val yylineno = ref(yygetlineNo(!(yystrm)))
                        in
                          (case (!(yyss))
                           of _ => (UserDeclarations.eof())
                          (* end case *))
                        end
                      else yystuck(lastMatch)
                else if inp = 0wx66
                  then yyQ42(strm', lastMatch)
                else if ULexBuffer.eof(!(yystrm))
                  then let
                    val yycolno = ref(yygetcolNo(!(yystrm)))
                    val yylineno = ref(yygetlineNo(!(yystrm)))
                    in
                      (case (!(yyss))
                       of _ => (UserDeclarations.eof())
                      (* end case *))
                    end
                  else yystuck(lastMatch)
            else if inp = 0wx7B
              then yyQ45(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx74
                  then yyQ44(strm', lastMatch)
                else if ULexBuffer.eof(!(yystrm))
                  then let
                    val yycolno = ref(yygetcolNo(!(yystrm)))
                    val yylineno = ref(yygetlineNo(!(yystrm)))
                    in
                      (case (!(yyss))
                       of _ => (UserDeclarations.eof())
                      (* end case *))
                    end
                  else yystuck(lastMatch)
            else if inp = 0wx7D
              then yyQ46(strm', lastMatch)
            else if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ24(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ24(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ24(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ24(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ24(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ24(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ23(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ23(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ23(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ23(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= 0wx66
              then yyQ23(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ28(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ28(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ28(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ28(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ28(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ28(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ27(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ27(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ27(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ27(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ27(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ27(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ26(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ26(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ26(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ26(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ26(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ26(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ25(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ25(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ25(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ25(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ25(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= 0wx66
              then yyQ25(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ29(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction25(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ29(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx63
              then yyQ8(strm', lastMatch)
            else if inp < 0wx63
              then if inp = 0wx27
                  then yyQ12(strm', lastMatch)
                else if inp < 0wx27
                  then if inp = 0wxD
                      then yyQ10(strm', lastMatch)
                    else if inp < 0wxD
                      then if inp = 0wxA
                          then yyQ9(strm', lastMatch)
                          else yyQ8(strm', lastMatch)
                    else if inp = 0wx22
                      then yyQ11(strm', lastMatch)
                      else yyQ8(strm', lastMatch)
                else if inp = 0wx5C
                  then yyQ14(strm', lastMatch)
                else if inp < 0wx5C
                  then if inp = 0wx30
                      then yyQ13(strm', lastMatch)
                      else yyQ8(strm', lastMatch)
                else if inp = 0wx62
                  then yyQ15(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp = 0wx74
              then yyQ19(strm', lastMatch)
            else if inp < 0wx74
              then if inp = 0wx6E
                  then yyQ17(strm', lastMatch)
                else if inp < 0wx6E
                  then if inp = 0wx66
                      then yyQ16(strm', lastMatch)
                      else yyQ8(strm', lastMatch)
                else if inp = 0wx72
                  then yyQ18(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp = 0wx78
              then yyQ22(strm', lastMatch)
            else if inp < 0wx78
              then if inp = 0wx76
                  then yyQ21(strm', lastMatch)
                else if inp = 0wx75
                  then yyQ20(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp = 0wx2028
              then yyQ9(strm', lastMatch)
            else if inp < 0wx2028
              then yyQ8(strm', lastMatch)
            else if inp <= 0wx2029
              then yyQ9(strm', lastMatch)
              else yyQ8(strm', lastMatch)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyQ31(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < 0wx28
              then if inp = 0wx27
                  then yyAction30(strm, yyNO_MATCH)
                  else yyQ31(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction30(strm, yyNO_MATCH)
              else yyQ31(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyQ31(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < 0wx28
              then if inp = 0wx27
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ31(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction12(strm, yyNO_MATCH)
              else yyQ31(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx27
              then yyAction12(strm, yyNO_MATCH)
            else if inp < 0wx27
              then if inp = 0wxA
                  then yyQ32(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction12(strm, yyNO_MATCH)
              else yyQ31(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx27
              then yyQ34(strm', lastMatch)
            else if inp < 0wx27
              then if inp = 0wxB
                  then yyQ31(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ32(strm', lastMatch)
                      else yyQ31(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ33(strm', lastMatch)
                  else yyQ31(strm', lastMatch)
            else if inp = 0wx5D
              then yyQ31(strm', lastMatch)
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyQ7(strm', lastMatch)
                  else yyQ31(strm', lastMatch)
            else if inp = 0wx2028
              then yyQ32(strm', lastMatch)
            else if inp < 0wx2028
              then yyQ31(strm', lastMatch)
            else if inp <= 0wx2029
              then yyQ32(strm', lastMatch)
              else yyQ31(strm', lastMatch)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ3(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ3(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction29(strm, yyNO_MATCH)
              else yyQ3(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ3(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ3(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction12(strm, yyNO_MATCH)
              else yyQ3(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyAction12(strm, yyNO_MATCH)
            else if inp < 0wx22
              then if inp = 0wxA
                  then yyQ4(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyQ3(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction12(strm, yyNO_MATCH)
              else yyQ3(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyQ6(strm', lastMatch)
            else if inp < 0wx22
              then if inp = 0wxB
                  then yyQ3(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ4(strm', lastMatch)
                      else yyQ3(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ5(strm', lastMatch)
                  else yyQ3(strm', lastMatch)
            else if inp = 0wx5D
              then yyQ3(strm', lastMatch)
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyQ7(strm', lastMatch)
                  else yyQ3(strm', lastMatch)
            else if inp = 0wx2028
              then yyQ4(strm', lastMatch)
            else if inp < 0wx2028
              then yyQ3(strm', lastMatch)
            else if inp <= 0wx2029
              then yyQ4(strm', lastMatch)
              else yyQ3(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of DQ => yyQ0(!(yystrm), yyNO_MATCH)
    | SQ => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()-1), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm 
(yyarg as  buf: CB.buf)(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
yyarg(yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm 
yyarg(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end

