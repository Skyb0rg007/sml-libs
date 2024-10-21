
structure JSON5Parser =
struct

structure J = JSON

exception SyntaxError of {line: int, column: int, reason: string}

datatype 'a state = State of {
      (* Reader state *)
      getc: 'a -> (char * 'a) option,
      stream: 'a,
      (* Error reporting *)
      line: int,
      column: int
   }

val eof = UTF8.maxCodePoint + 0w1

fun isSpace (c: UTF8.wchar): bool =
          c = 0wx0009 (* Horizontal Tab *)
   orelse c = 0wx000a (* Line Feed *)
   orelse c = 0wx000b (* Vertical Tab *)
   orelse c = 0wx000c (* Form Feed *)
   orelse c = 0wx000d (* Carriage Return *)
   orelse c = 0wx0020 (* Space *)
   orelse c = 0wx00a0 (* No-Break Space *)
   orelse c = 0wx1680 (* Ogham Space Mark *)
   orelse (0wx2000 <= c andalso c <= 0wx200a) (* Letterpress Typesetting *)
   orelse c = 0wx2028 (* Line Separator *)
   orelse c = 0wx2029 (* Paragraph Separator *)
   orelse c = 0wx202f (* Narrow No-Break Space *)
   orelse c = 0wx205f (* Medium Mathematical Space *)
   orelse c = 0wx3000 (* Ideographic Space *)
   orelse c = 0wxfeff (* Zero Width No-Break Space *)

fun getu (State {getc, stream, line, column}) =
   case UTF8.getu getc stream of
      NONE => (eof, State {
         getc = getc,
         stream = stream,
         line = line,
         column = column + 1
      })
    | SOME (c, stream') =>
         let
            val (line', column') =
               if c = 0wx0a
                  then (line + 1, 0)
               else (line, column + 1)
         in
            (c,
             State {
               getc = getc,
               stream = stream',
               line = line',
               column = column'
             })
         end

fun syntaxError (State {line, column, ...}) reason =
   raise SyntaxError {line = line, column = column, reason = reason}

fun parseString state quote =
   let
      val buf = CharBuffer.new 16
      fun go stream =
         case getu state of
            (c, state') =>
               if c = quote
                  then (SOME (CharBuffer.contents buf), state')
               else if c < 0wx20
                  then raise syntaxError state' "unexpected control character"
               else if c = 0wx5c (* \ *)
                  then
                     case getu state' of
                        (0wx27 (* ' *), state'') =>
                           (CharBuffer.add1 (buf, #"'"); go state'')
                      | (0wx22 (* " *), state'') =>
                           (CharBuffer.add1 (buf, #"\""); go state'')
                      | (0wx2f (* / *), state'') =>
                           (CharBuffer.add1 (buf, #"/"); go state'')
                      | (0wx62 (* b *), state'') =>
                           (CharBuffer.add1 (buf, #"\b"); go state'')
                      | (0wx66 (* f *), state'') =>
                           (CharBuffer.add1 (buf, #"\f"); go state'')
                      | (0wx6e (* n *), state'') =>
                           (CharBuffer.add1 (buf, #"\n"); go state'')
                      | (0wx72 (* r *), state'') =>
                           (CharBuffer.add1 (buf, #"\r"); go state'')
                      | (0wx74 (* t *), state'') =>
                           (CharBuffer.add1 (buf, #"\t"); go state'')
                      | (0wx75 (* u *), state'') =>
                           raise Fail "NYI"
                      | (0wx0a (* \n *), state'') => go state''
                      | (0wx0d (* \r *), state'') =>
                           if #1 (getu state'') = 0wx0a (* \n *)
                              then go state''
                           else syntaxError state'' "unexpected character"
                      | (c, state'') =>
                           if c = eof
                              then syntaxError state'' "unexpected end of input"
                           else syntaxError state'' "unexpected character"
               else (CharBuffer.addVec (buf, UTF8.encode c); go state')
   in
      go state
   end

end

(* vim: set tw=0 ts=3 sw=3: *)

