
signature LEXER =
  sig
    type stream

    val lex : stream -> (Tokens.t * stream -> 'a) -> 'a
  end

structure Lexer :> LEXER where type stream = char list =
  struct
    type stream = char list

    exception LexError of char

    fun span _ [] = ([], [])
      | span p (x :: xs) =
        if p x
          then let val (ys, zs) = span p xs in (x :: ys, zs) end
        else ([], x :: xs)

    fun lex [] k = k (Tokens.EOF, [])
      | lex (#"(" :: cs) k = k (Tokens.LPAREN, cs)
      | lex (#")" :: cs) k = k (Tokens.RPAREN, cs)
      | lex (#"=" :: cs) k = k (Tokens.EQ, cs)
      | lex (#"+" :: cs) k = k (Tokens.PLUS, cs)
      | lex (#";" :: cs) k = k (Tokens.SEMI, cs)
      | lex (c :: cs) k =
        if Char.isSpace c
          then lex cs k
        else if Char.isDigit c
          then
            let
              val (ds, cs) = span Char.isDigit cs
              val n = List.foldl (fn (c, n) => 10 * n + Char.ord c - Char.ord #"0") 0 (c :: ds)
            in
              k (Tokens.NUM n, cs)
            end
        else if Char.isAlpha c
          then
            let
              val (is, cs) = span Char.isAlphaNum cs
              val id = String.implode (c :: is)
            in
              if id = "fun"
                then k (Tokens.FUN, cs)
              else if id = "val"
                then k (Tokens.VAL, cs)
              else k (Tokens.ID id, cs)
            end
        else raise LexError c
  end
