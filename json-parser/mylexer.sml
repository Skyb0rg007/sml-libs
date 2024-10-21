
structure MyLexer:
   sig
      type stream

      val streamifyInstream: TextIO.instream -> stream
   end =
   struct
      type tok = JSONTokens.token
      type span = AntlrStreamPos.span
      type pos = AntlrStreamPos.pos

      type state = Word8.word
      val INITIAL: state = 0w1
      val STRING: state = 0w3

      datatype stream = STRM of {
         stream: ULexBuffer.stream,
         state: state
      }

      fun streamifyInstream strm =
         STRM {
            stream = ULexBuffer.mkStream (0, fn () => TextIO.input strm),
            state = INITIAL
         }

      val accept: Word8Vector.vector =
         Byte.stringToBytes
         "\000\
         \\000\000\000\000\026\025\001\015\006\025\
         \\011\011\007\004\005\025\025\025\002\003\
         \\023\024\025\011\011\000\000\011\000\000\
         \\000\023\016\017\018\019\020\021\000\012\
         \\000\013\000\000\000\000\000\000\008\009\
         \\000\000\014\010\000\022\000"

      (* Maps input byte to equivalence class *)
      val ec: Word8Vector.vector =
         Byte.stringToBytes
         "\000\
         \\001\001\001\001\001\001\001\001\002\002\
         \\001\001\002\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\002\001\003\001\001\001\001\001\001\
         \\001\001\004\005\006\007\001\008\009\009\
         \\009\009\009\009\009\009\009\010\001\001\
         \\001\001\001\001\011\011\011\011\012\011\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\013\014\015\001\001\001\016\017\011\011\
         \\018\019\001\001\001\001\001\020\001\021\
         \\001\001\001\022\023\024\025\001\001\001\
         \\001\001\026\001\027\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001"

      (* Maps equivalence class to offset *)
      val meta: Word8Vector.vector =
         Byte.stringToBytes
         "\000\
         \\001\001\002\001\001\001\001\003\003\001\
         \\003\003\001\002\001\003\003\003\003\001\
         \\001\001\001\001\001\001\001"

      val base: Word8Vector.vector =
         Byte.stringToBytes
         "\000\
         \\000\000\025\026\093\094\094\094\094\022\
         \\025\037\094\094\094\076\066\068\094\094\
         \\000\094\042\029\000\025\044\000\069\067\
         \\051\000\094\094\094\094\094\094\000\060\
         \\049\062\042\042\042\000\071\036\094\094\
         \\000\065\073\094\000\094\094\082\085\048\
         \\039\035\032"

      val def: Word8Vector.vector =
         Byte.stringToBytes
         "\000\
         \\057\001\058\058\057\057\057\057\057\057\
         \\057\057\057\057\057\057\057\057\057\057\
         \\059\057\057\057\012\057\057\012\057\057\
         \\057\059\057\057\057\057\057\057\060\057\
         \\057\057\057\057\057\061\057\057\057\057\
         \\062\057\057\057\063\057\000\057\057\057\
         \\057\057\057"

      val nxt: Word8Vector.vector =
         Byte.stringToBytes
         "\000\
         \\006\007\008\006\009\010\006\011\012\013\
         \\006\006\014\006\015\006\006\006\016\006\
         \\017\006\006\018\006\019\020\022\022\024\
         \\025\026\040\040\056\026\027\055\023\023\
         \\027\051\027\026\028\028\027\041\027\041\
         \\046\042\042\054\027\033\042\042\034\050\
         \\035\049\036\037\048\038\039\040\040\042\
         \\042\047\053\053\052\045\052\047\053\053\
         \\053\053\021\021\021\032\044\032\043\031\
         \\030\029\057\005\057\057\057\057\057\057\
         \\057\057\057\057\057\057\057\057\057\057\
         \\057\057\057\057\057\057\057\057\057\057\
         \\057"

      val chk: Word8Vector.vector =
         Byte.stringToBytes
         "\000\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\001\001\001\
         \\001\001\001\001\001\001\001\003\004\010\
         \\010\011\026\026\063\024\011\062\003\004\
         \\024\061\011\012\012\012\024\027\012\027\
         \\060\027\027\048\012\023\041\041\023\045\
         \\023\044\023\023\043\023\023\040\040\042\
         \\042\040\052\052\047\031\047\040\047\047\
         \\053\053\058\058\058\059\030\059\029\018\
         \\017\016\005\057\057\057\057\057\057\057\
         \\057\057\057\057\057\057\057\057\057\057\
         \\057\057\057\057\057\057\057\057\057\057\
         \\057"

      val () =
         if Word8Vector.length accept <> 58
            then raise Fail "accept"
         else ()
      val () =
         if Word8Vector.length ec <> 256
            then raise Fail "ec"
         else ()
      val () =
         if Word8Vector.length meta <> 28
            then raise Fail "meta"
         else ()
      val () =
         if Word8Vector.length base <> 64
            then raise Fail "base"
         else ()
      val () =
         if Word8Vector.length def <> 64
            then raise Fail "def"
         else ()
      val () =
         if Word8Vector.length nxt <> 122
            then raise Fail "nxt"
         else ()
      val () =
         if Word8Vector.length chk <> 122
            then raise Fail "chk"
         else ()

  (* datatype stream = ... *)
  (* exception Incomplete *)
  (* val mkStream : AntlrStreamPos.pos * (unit -> string) -> stream *)
  (* val getc : stream -> (char * stream) option *)
  (* val getu : stream -> (word * stream) option *)
  (* val getpos : stream -> AntlrStreamPos.pos *)
  (* val subtract : stream * stream -> substring *)
  (* val eof : stream -> bool *)
  (* val lastWasNL : stream -> bool *)

      fun yymatch (lastAccept, stream, state) =
         case ULexBuffer.getc stream of
            NONE => NONE
          | SOME (c, stream') =>
               let
                  val lastAccept = ref lastAccept
                  val i = Char.ord c
                  val class: int ref = ref (Word8.toInt (Word8Vector.sub (ec, i)))
                  val cur: Word8.word ref = ref state
               in
                  if Word8Vector.sub (accept, Word8.toInt (!cur)) <> 0w0
                     then (lastAccept := stream)
                  else ();
                  while Word8Vector.sub (chk, Word8.toInt (Word8Vector.sub (base, Word8.toInt (!cur))) + !class) <> !cur do (
                     cur := Word8Vector.sub (def, Word8.toInt (!cur));
                     if !cur >= 0w58
                        then (class := Word8.toInt (Word8Vector.sub (meta, !class)))
                     else ()
                  );
                  if Word8Vector.sub (base, Word8.toInt (!cur)) = 0w94
                     then yymatch (!lastAccept, stream, !cur)
                  else SOME (lastAccept, !cur, stream')
               end

      fun innerLex (stream, state, srcMap) =
         let
         in
            raise Empty
         end

      fun lex srcMap (STRM {stream, state}) =
         let
            val (tok, span, stream', state') = innerLex (stream, state, srcMap)
         in
            (tok, span, STRM {stream = stream', state = state'})
         end

   end

(* vim: set tw=0 ts=3 sw=3: *)
