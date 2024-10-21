
%name JSON5Lexer;

%arg (buf: CharBuffer.buf);

%defs (
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
);

%let LF = "\n";
%let CR = "\r";
%let LS = "\u2028";
%let PS = "\u2029";
%let digit = [0-9];
%let xdigit = {digit}|[a-fA-F];
%let LineTerm = {LF}|{CR}|{CR}{LF}|{LS}|{PS};

%states INITIAL SQ DQ;

<INITIAL>[\ \t\n\r]+ => ( skip() );
<INITIAL>"{"         => ( T.LBRACE );
<INITIAL>"}"         => ( T.RBRACE );
<INITIAL>"["         => ( T.LBRACK );
<INITIAL>"]"         => ( T.RBRACK );
<INITIAL>","         => ( T.COMMA );
<INITIAL>":"         => ( T.COLON );
<INITIAL>"null"      => ( T.NULL );
<INITIAL>"true"      => ( T.TRUE );
<INITIAL>"false"     => ( T.FALSE );

(* Strings *)
<INITIAL>"\"" => ( YYBEGIN DQ; CB.clear buf; continue () );
<INITIAL>"'"  => ( YYBEGIN SQ; CB.clear buf; continue () );
(* SourceCharacter but not one of " or \ or LineTerminator *)
<DQ,SQ>{LineTerm} => (
    T.ERROR (String.concat
        ["unescaped line terminator '", String.toString yytext, "'"])
);
<DQ>"\"" => ( YYBEGIN INITIAL; T.STRING (CB.contents buf) before CB.clear buf );
<SQ>"'"  => ( YYBEGIN INITIAL; T.STRING (CB.contents buf) before CB.clear buf );
(* EscapeSequence *)
<DQ,SQ>"\\\'" => ( CB.add1 (buf, #"'"); continue () );
<DQ,SQ>"\\\"" => ( CB.add1 (buf, #"\""); continue () );
<DQ,SQ>"\\\\" => ( CB.add1 (buf, #"\\"); continue () );
<DQ,SQ>"\\b" => ( CB.add1 (buf, #"\b"); continue () );
<DQ,SQ>"\\f" => ( CB.add1 (buf, #"\f"); continue () );
<DQ,SQ>"\\n" => ( CB.add1 (buf, #"\n"); continue () );
<DQ,SQ>"\\r" => ( CB.add1 (buf, #"\r"); continue () );
<DQ,SQ>"\\t" => ( CB.add1 (buf, #"\t"); continue () );
<DQ,SQ>"\\v" => ( CB.add1 (buf, #"\v"); continue () );
<DQ,SQ>"\\0"{digit} => ( T.ERROR "unrecognized escape sequence" );
<DQ,SQ>"\\0" => ( CB.add1 (buf, #"\000"); continue () );
<DQ,SQ>"\\x"{xdigit}{2} => (
    CB.addVec (buf, UTF8.encode (hex (Substring.triml 2 yysubstr)));
    continue ()
);
<DQ,SQ>"\\u"{xdigit}{4} => (
    CB.addVec (buf, UTF8.encode (hex (Substring.triml 2 yysubstr)));
    continue ()
);
<DQ,SQ>"\\". => ( CB.add1 (buf, Substring.sub (yysubstr, 1)); continue () );
<DQ>[^\\"]+ => ( CB.addSlice (buf, yysubstr); continue () );
<SQ>[^\\']+ => (
print ("adding slice " ^ yytext ^ "\n");
print ("adding slice " ^ Substring.string yysubstr ^ "\n");
CB.addSlice (buf, yysubstr); continue () );
(* LineContinuation *)
<DQ,SQ>"\\"{LineTerm} => ( continue () );
