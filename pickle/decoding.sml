
structure Decoding =
struct

datatype element =
   ArrayBegin
 | ArrayEnd of substring
 | ObjectBegin
 | ObjectEnd of substring
 | StringContent of substring
 | StringRaw of substring * bool
 | StringEnd
 | JInteger of IntInf.int

datatype token_result =
   TokMoreData of substring -> token_result
 | PartialResult of element * (unit -> token_result)
 | TokFailed

datatype 'a parser = Parser of token_result -> 'a parse_result

and 'a parse_result =
   MoreData of 'a parser * (substring -> token_result)
 | Failed of string
 | Done of substring * (unit -> token_result)
 | Yield of 'a * (unit -> 'a parse_result) (* XXX *)

fun callParse (Parser p) = p

fun test (Parser p, toks) =
   let
      val tr =
         List.foldr
            (fn (t, acc) =>
               PartialResult (t, fn _ => acc))
            TokFailed
            toks
   in
      p tr
   end

fun moreData' p tok =
   case tok of
      PartialResult (el, ntok) => p (tok, el, ntok)
    | TokMoreData ntok => MoreData (Parser (moreData' p), ntok)
    | TokFailed => Failed "More data - lexer failed."

fun moreData p tok = moreData' p (tok ())

local
   val empty = Substring.full ""

   fun handleLongString lvl (_, StringContent _, ntok) =
      moreData (handleLongString lvl) ntok
     | handleLongString 0 (_, StringEnd, ntok) =
      Done (empty, ntok)
     | handleLongString lvl (_, StringEnd, ntok) =
      moreData (handleLongString lvl) ntok
     | handleLongString _ (_, el, _) =
      Failed ("Unexpected element in handleLongString")

   fun handleTok 0 (_, StringRaw _, ntok) = Done (empty, ntok)
     | handleTok 0 (_, JInteger _, ntok) = Done (empty, ntok)
     | handleTok 0 (_, ArrayEnd _, ntok) = Done (empty, ntok)
     | handleTok 0 (_, ObjectEnd _, ntok) = Done (empty, ntok)
     | handleTok 1 (_, ArrayEnd ctx, ntok) = Done (ctx, ntok)
     | handleTok 1 (_, ObjectEnd ctx, ntok) = Done (ctx, ntok)
     | handleTok lvl (_, el, ntok) =
      case el of
         StringContent _ => moreData (handleLongString lvl) ntok
       | JInteger _ => moreData (handleTok lvl) ntok
       | StringRaw _ => moreData (handleTok lvl) ntok
       | ArrayEnd _ => moreData (handleTok (lvl - 1)) ntok
       | ObjectEnd _ => moreData (handleTok (lvl - 1)) ntok
       | ArrayBegin => moreData (handleTok (lvl + 1)) ntok
       | ObjectBegin => moreData (handleTok (lvl + 1)) ntok
       | StringEnd => Failed "Internal error - out of order StringEnd"
in
   fun ignoreVal' n = Parser (moreData' (handleTok n))

   val ignoreVal = Parser (fn t => moreData' (handleTok 0) t)
end

fun pure x =
   let
      fun process (Failed err) = Failed err
        | process (Done (ctx, tok)) = Yield (x, fn () => Done (ctx, tok))
        | process (MoreData (np, ntok)) = MoreData (Parser (process o callParse np), ntok)
        | process _ = Failed "pure: internal error"
   in
      Parser (fn tok => process (callParse ignoreVal tok))
   end

fun both (Parser p1, Parser p2) =
   let
      fun process ([], _, Done (ctx, ntok), _) = Done (ctx, ntok)
        | process (l1, l2, Yield (v, np1), p2) =
         process (v :: l1, l2, np1 (), p2)
        | process (l1, l2, p1, Yield (v, np2)) =
         process (l1, v :: l2, p1, np2 ())
        | process (l1, l2, Done (ctx, ntok), Done _) =
         let
            fun loop1 ([], acc) = acc
              | loop1 (mx :: l1, acc) = loop2 (mx, l1, l2, acc)
            and loop2 (_, l1, [], acc) = loop1 (l1, acc)
              | loop2 (mx, l1, my :: l2, acc) =
               Yield ((mx, my), fn () => loop2 (mx, l1, l2, acc))
         in
            loop1 (l1, Done (ctx, ntok))
         end
        | process (l1, l2, MoreData (Parser np1, ntok1), MoreData (Parser np2, _)) =
         MoreData (Parser (fn tok => process (l1, l2, np1 tok, np2 tok)), ntok1)
        | process (_, _, Failed err, _) = Failed err
        | process (_, _, _, Failed err) = Failed err
        | process _ = Failed "both: unexpected error"
   in
      Parser (fn tok => process ([], [], p1 tok, p2 tok))
   end

local
   fun object' {once, parseValue} =
      let
         fun nextItem _ (_, ObjectEnd ctx, ntok) = Done (ctx, ntok)
           | nextItem y (_, StringRaw (str, true), ntok) =
            objContent y (callParse (parseValue str) (ntok ()))
           | nextItem _ _ = Failed "NYI"

         and objContent yielded (Done (_, ntp)) =
            if once andalso yielded
               then callParse (ignoreVal' 1) (ntp ())
            else moreData (nextItem yielded) ntp
           | objContent yielded (MoreData (Parser np, ntok)) =
            MoreData (Parser (objContent yielded o np), ntok)
           | objContent _ (Yield (v, np)) =
            Yield (v, fn () => objContent true (np ()))
           | objContent _ (Failed err) = Failed err

         fun process (PartialResult (ObjectBegin, ntp)) =
            moreData (nextItem false) ntp
           | process (tp as PartialResult _) =
            callParse ignoreVal tp
           | process (TokMoreData ntok) =
            MoreData (object' {once=once,parseValue=parseValue}, ntok)
           | process TokFailed = Failed "Object - token failed"
      in
         Parser process
      end
in
end

end

(* vim: set ts=3 sw=3: *)
