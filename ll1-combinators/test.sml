
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

      fun toInt MUL = 0
        | toInt DIV = 1
        | toInt ADD = 2
        | toInt SUB = 3
        | toInt SEMI = 4
        | toInt NUM = 5
        | toInt LPAREN = 6
        | toInt RPAREN = 7
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

structure LL1 = ParserFn(
   struct
      structure Kind = Kind
      structure Token = Token
   end)

datatype ast =
   Num of int
 | Add of ast * ast
 | Sub of ast * ast
 | Mul of ast * ast
 | Div of ast * ast

fun astToString (Num n) = Int.toString n
  | astToString (Add (a, b)) = "(+ " ^ astToString a ^ " " ^ astToString b ^ ")"
  | astToString (Sub (a, b)) = "(- " ^ astToString a ^ " " ^ astToString b ^ ")"
  | astToString (Mul (a, b)) = "(* " ^ astToString a ^ " " ^ astToString b ^ ")"
  | astToString (Div (a, b)) = "(/ " ^ astToString a ^ " " ^ astToString b ^ ")"

local
   open LL1

   val DIV = kind Kind.DIV
   val MUL = kind Kind.MUL
   val ADD = kind Kind.ADD
   val SUB = kind Kind.SUB
   val LPAREN = kind Kind.LPAREN
   val RPAREN = kind Kind.RPAREN
   val SEMI = kind Kind.SEMI
   val NUM = map (fn Token.NUM n => n | _ => raise Fail "") (kind Kind.NUM)

   val op + = choice
in
   val expr = fix (fn expr =>
      let
         val factor: ast LL1.t =
            map Num NUM
          + map2 (fn (_, n) => Num (~n)) (SUB, NUM)
          + map3 (fn (_, e, _) => e) (LPAREN, expr, RPAREN)

         val term_op: (ast -> ast) LL1.t = fix (fn term_op =>
            map3 (fn (_, y, f) => fn x => Mul (x, f y)) (MUL, factor, term_op)
          + map3 (fn (_, y, f) => fn x => Div (x, f y)) (DIV, factor, term_op)
          + pure (fn x => x))

         val term: ast LL1.t = map2 (fn (x, f) => f x) (factor, term_op)

         val expr_op: (ast -> ast) LL1.t = fix (fn expr_op =>
            map3 (fn (_, y, f) => fn x => Add (x, f y)) (ADD, term, expr_op)
          + map3 (fn (_, y, f) => fn x => Sub (x, f y)) (SUB, term, expr_op)
          + pure (fn x => x))
      in
         map2 (fn (t, e) => e t) (term, expr_op)
      end)

   fun parse str =
      let
         val toks = Token.tokenize str
      in
         print ("[" ^ String.concatWith "," (List.map Token.toString toks) ^ "]\n");
         case LL1.parse (expr, toks) of
            NONE => print "No parse\n"
          | SOME x => print (astToString x ^ "\n")
      end
end

end

(* vim: set tw=0 sw=3 ts=3: *)
