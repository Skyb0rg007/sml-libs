
structure Test =
struct

fun runSMLNJ filename =
   let
      val startTime = Timer.startCPUTimer ()
      val srcMap = AntlrStreamPos.mkSourcemap ()
      val instream = TextIO.openIn filename
      val stream = JSONLexer.streamifyInstream instream

      fun loop (stream, n) =
         let
            val (tok, span, stream') = JSONLexer.lex srcMap stream
         in
            case tok of
               JSONTokens.EOF => n
             | _ => loop (stream', n + 1)
         end

      val n = loop (stream, 0)
   in
      TextIO.closeIn instream;
      case Timer.checkCPUTimer startTime of
        {usr, sys} => (
          print ("usr = " ^ Time.fmt 2 usr ^ "\n");
          print ("sys = " ^ Time.fmt 2 sys ^ "\n")
      );
      print ("n = " ^ Int.toString n ^ "\n")
   end

fun runSMLNJNoMemo filename =
   let
      val startTime = Timer.startCPUTimer ()
      val srcMap = AntlrStreamPos.mkSourcemap ()
      val instream = TextIO.openIn filename
      val stream = JSONLexerNoMemo.streamifyInstream instream

      fun loop (stream, n) =
         let
            val (tok, span, stream') = JSONLexerNoMemo.lex srcMap stream
         in
            case tok of
               JSONTokens.EOF => n
             | _ => loop (stream', n + 1)
         end

      val n = loop (stream, 0)
   in
      TextIO.closeIn instream;
      case Timer.checkCPUTimer startTime of
        {usr, sys} => (
          print ("usr = " ^ Time.fmt 2 usr ^ "\n");
          print ("sys = " ^ Time.fmt 2 sys ^ "\n")
      );
      print ("n = " ^ Int.toString n ^ "\n")
   end

fun runCustom filename =
   let
      val srcMap = AntlrStreamPos.mkSourcemap ()
      val instream = TextIO.openIn filename
      val stream = MyLexer.streamifyInstream instream
   in
      ()
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
