
structure Tokens =
struct
  datatype t =
      VAL
    | FUN
    | LPAREN
    | RPAREN
    | ID of string
    | EQ
    | NUM of int
    | PLUS
    | SEMI
    | EOF

  fun toString VAL = "val"
    | toString FUN = "fun"
    | toString LPAREN = "("
    | toString RPAREN = ")"
    | toString (ID x) = x
    | toString EQ = "="
    | toString (NUM n) = Int.toString n
    | toString PLUS = "+"
    | toString SEMI = ";"
    | toString EOF = "<EOF>"

  val all = [VAL, FUN, LPAREN, RPAREN, ID "x", EQ, NUM 0, PLUS, SEMI]
end
