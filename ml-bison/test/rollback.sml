
structure WrappedLexer :
  sig
    type stream

    val lift : Lexer.stream -> stream
    val lex : stream -> (Tokens.t * stream -> Parser.statement) -> Parser.statement

    val parse : Lexer.stream -> Parser.statement * string option
  end =
  struct
    structure Window =
      struct
        datatype t = W of (Tokens.t -> Parser.statement) list

        val k = 3
        val empty = W []
        fun push (W xs, x) = W (List.take (xs @ [x], k))
          handle Subscript => W (xs @ [x])
        fun toList (W xs) = xs
      end

    type stream = Lexer.stream * Window.t option

    exception ParseError of stream list

    fun checkpoints (_, NONE) = []
      | checkpoints (_, SOME w) = Window.toList w

    fun lex (s, NONE) k =
          Lexer.lex s (fn (t, s') =>
            k (t, (s', NONE))
            handle Parser.ParseError => raise ParseError [(s, NONE)])
      | lex (s, SOME w) k =
          Lexer.lex s (fn (t, s') =>
            k (t, (s', SOME (Window.push (w, fn t' =>
              k (t', (s', NONE))
              handle Parser.ParseError => raise ParseError []))))
            handle Parser.ParseError => raise ParseError [(s, SOME w)])

    fun lift s = (s, SOME Window.empty)

    fun tryReps _ [] = NONE
      | tryReps k (t :: ts) = SOME (k t, t)
        handle ParseError _ => tryReps k ts

    fun tryCPs [] = NONE
      | tryCPs (k :: ks) =
        case tryReps k Tokens.all of
            NONE => tryCPs ks
          | SOME rt => SOME rt

    fun parse s =
      (Parser.parse lex (lift s), NONE)
      handle ParseError wss =>
        case tryCPs (List.concatMap checkpoints wss) of
            NONE => raise Parser.ParseError
          | SOME (r, t) => (r, SOME ("Did you mean " ^ Tokens.toString t ^ "?"))
        
  end
