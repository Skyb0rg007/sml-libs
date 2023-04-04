
structure Test =
struct

structure Kind =
   struct
      datatype t =
         MUL
       | DIV
       | ADD
       | SUB
       | SEMI
       | NUM
       | LPAREN
       | RPAREN

      fun hash MUL = 0w0
        | hash DIV = 0w1
        | hash ADD = 0w2
        | hash SUB = 0w3
        | hash SEMI = 0w4
        | hash NUM = 0w5
        | hash LPAREN = 0w6
        | hash RPAREN = 0w7

      fun compare (a, b) = Word.compare (hash a, hash b)
   end

structure Token =
   struct
      datatype t =
         MUL
       | DIV
       | ADD
       | SUB
       | SEMI
       | NUM of int
       | LPAREN
       | RPAREN

      fun kind MUL = Kind.MUL
        | kind DIV = Kind.DIV
        | kind ADD = Kind.ADD
        | kind SUB = Kind.SUB
        | kind SEMI = Kind.SEMI
        | kind (NUM _) = Kind.NUM
        | kind LPAREN = Kind.LPAREN
        | kind RPAREN = Kind.RPAREN


      fun toString MUL = "MUL"
        | toString DIV = "DIV"
        | toString ADD = "ADD"
        | toString SUB = "SUB"
        | toString SEMI = "SEMI"
        | toString (NUM n) = "NUM " ^ Int.toString n
        | toString LPAREN = "LPAREN"
        | toString RPAREN = "RPAREN"

      fun tokenize str =
         let
            fun f (c, (SOME n, acc)) =
               if Char.isDigit c
                  then (SOME (10 * n + (Char.ord c - Char.ord #"0")), acc)
                  else f (c, (NONE, NUM n :: acc))
              | f (c, (NONE, acc)) =
               case c of
                  #"(" => (NONE, LPAREN :: acc)
                | #")" => (NONE, RPAREN :: acc)
                | #"*" => (NONE, MUL :: acc)
                | #"/" => (NONE, DIV :: acc)
                | #"+" => (NONE, ADD :: acc)
                | #"-" => (NONE, SUB :: acc)
                | #";" => (NONE, SEMI :: acc)
                | _ =>
                  if Char.isSpace c
                     then (NONE, acc)
                  else if Char.isDigit c
                     then (SOME (Char.ord c - Char.ord #"0"), acc)
                  else raise Fail ("Invalid char " ^ Char.toString c)
         in
            case CharVector.foldl f (NONE, []) str of
               (NONE, toks) => List.rev toks
             | (SOME n, toks) => List.rev (NUM n :: toks)
         end
   end

structure Nonterm =
   struct
      datatype t =
         Num of int
       | Add of t * t
       | Sub of t * t
       | Mul of t * t
       | Div of t * t
       | Seq of t * t
       | T of Token.t
       | P of t list

      fun toString (Num n) = Int.toString n
        | toString (Add (a, b)) = "(+ " ^ toString a ^ " " ^ toString b ^ ")"
        | toString (Sub (a, b)) = "(- " ^ toString a ^ " " ^ toString b ^ ")"
        | toString (Mul (a, b)) = "(* " ^ toString a ^ " " ^ toString b ^ ")"
        | toString (Div (a, b)) = "(/ " ^ toString a ^ " " ^ toString b ^ ")"
        | toString (Seq (a, b)) = "(begin " ^ toString a ^ " " ^ toString b ^ ")"
        | toString (T t) = "(token " ^ Token.toString t ^ ")"
        | toString (P ts) = "(list " ^ String.concatWith " " (map toString ts) ^ ")"

      val token = T
      fun (P x) + (P y) = P (x @ y)
        | x + (P y) = P (x :: y)
        | (P x) + y = P (x @ [y])
        | x + y = P [x, y]
   end

structure LL1 = ParserFn(
   struct
      structure Kind = Kind
      structure Token = Token
      structure Nonterm = Nonterm
   end)

local
   open LL1

   fun void _ = Nonterm.P []

   fun num (Nonterm.T (Token.NUM n)) = Nonterm.Num n
     | num _ = raise Fail "Not a number"

   val DIV = elem Kind.DIV
   val MUL = elem Kind.MUL
   val ADD = elem Kind.ADD
   val SUB = elem Kind.SUB
   val LPAREN = map void (elem Kind.LPAREN)
   val RPAREN = map void (elem Kind.RPAREN)
   val SEMI = map void (elem Kind.SEMI)
   val NUM = map num (elem Kind.NUM)

   fun f_expr (Nonterm.P (a :: Nonterm.T binop :: (rest as _::_))) =
      (case binop of
          Token.ADD => Nonterm.Add (a, f_expr (Nonterm.P rest))
        | Token.SUB => Nonterm.Sub (a, f_expr (Nonterm.P rest))
        | Token.MUL => Nonterm.Mul (a, f_expr (Nonterm.P rest))
        | Token.DIV => Nonterm.Div (a, f_expr (Nonterm.P rest))
        | t => raise Fail ("Invalid binop " ^ Token.toString t))
     | f_expr (Nonterm.P [a]) = a
     | f_expr n = raise Fail ("Invalid expr " ^ Nonterm.toString n)

   fun f_factor (n as Nonterm.Num _) = n
     | f_factor (Nonterm.P [Nonterm.T Token.SUB, Nonterm.Num n]) = Nonterm.Num (~n)
     | f_factor (Nonterm.P [a]) = a
     | f_factor n = raise Fail ("Invalid factor " ^ Nonterm.toString n)
in
   val expr = fix (fn expr =>
      let
         val factor = map f_factor (NUM + SUB * NUM + LPAREN * expr * RPAREN)
         val term_op = fix (fn term_op =>
               MUL * factor * term_op
             + DIV * factor * term_op
             + epsilon (Nonterm.P []))
         val term = map f_expr (factor * term_op)
         val expr_op = fix (fn expr_op =>
               ADD * term * expr_op
             + SUB * term * expr_op
             + epsilon (Nonterm.P []))
      in
         map f_expr (term * expr_op * SEMI)
      end)

   fun parse str =
      let
         val toks = Token.tokenize str
      in
         print ("[" ^ String.concatWith "," (List.map Token.toString toks) ^ "]\n");
         case LL1.parse (expr, toks) of
            NONE => print "No parse\n"
          | SOME x => print (Nonterm.toString x ^ "\n")
      end
end

end

(* vim: set tw=0 sw=3 ts=3: *)
