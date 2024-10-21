
structure Parser =
  struct
    datatype statement =
        Fun of string * string * int
      | Val of string * int

    type 'a parser = Lexer.stream -> 'a

    exception ParseError

    fun parseFun lex s =
      lex s (fn (Tokens.ID f, s) =>
      lex s (fn (Tokens.LPAREN, s) =>
      lex s (fn (Tokens.ID x, s) =>
      lex s (fn (Tokens.RPAREN, s) =>
      lex s (fn (Tokens.EQ, s) =>
      lex s (fn (Tokens.NUM n, s) =>
      lex s (fn (Tokens.SEMI, s) =>
      Fun (f, x, n)
              | _ => raise ParseError)
              | _ => raise ParseError)
              | _ => raise ParseError)
              | _ => raise ParseError)
              | _ => raise ParseError)
              | _ => raise ParseError)
              | _ => raise ParseError)

    fun parseVal lex s =
      lex s (fn (Tokens.ID x, s) =>
      lex s (fn (Tokens.EQ, s) =>
      lex s (fn (Tokens.NUM n, s) =>
      lex s (fn (Tokens.SEMI, s) =>
      Val (x, n)
              | _ => raise ParseError)
              | _ => raise ParseError)
              | _ => raise ParseError)
              | _ => raise ParseError)

    fun parse lex s =
      lex s
        (fn (Tokens.FUN, s) => parseFun lex s
          | (Tokens.VAL, s) => parseVal lex s
          | _ => raise ParseError)

    val parse : ('s -> (Tokens.t * 's -> statement) -> statement) -> 's -> statement = parse
  end
