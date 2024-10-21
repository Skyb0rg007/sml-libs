
structure Rad =
struct

structure G:
   sig
      datatype token =
         NUM of int
       | STAR
       | PLUS
       | LPAREN
       | RPAREN

      datatype exp =
         Mul of exp * term
       | Exp of exp * term
       | Term of term

      and term =
         Plus of term * factor
       | Fact of factor

      and factor =
         Paren of exp
       | Num of int

      val tokenToString: token -> string
      val entry: token list -> exp
   end =
   struct
      datatype token =
         NUM of int
       | STAR
       | PLUS
       | LPAREN
       | RPAREN

      fun tokenToString (NUM n) = Int.toString n
        | tokenToString STAR = "'*'"
        | tokenToString PLUS = "'+'"
        | tokenToString LPAREN = "'('"
        | tokenToString RPAREN = "')'"

      datatype exp =
         Mul of exp * term
       | Exp of exp * term
       | Term of term

      and term =
         Plus of term * factor
       | Fact of factor

      and factor =
         Paren of exp
       | Num of int

      (* Grammar
       *
       * [0] E* ::= E '$'
       * [1] E ::= E '*' T
       * [2] E ::= E '*' '*' T
       * [3] E ::= T
       * [4] T ::= T '+' F
       * [5] T ::= F
       * [6] F ::= '(' E ')'
       * [7] F ::= 'id'
       *
       * Recognition Points
       *
       * [0] E* ::= ⬤ E '$'
       * [1] E ::= E '*' ⬤ T
       * [2] E ::= E '*' ⬤ '*' T
       * [3] E ::= ⬤ T
       * [4] T ::= T ⬤ '+' F
       * [5] T ::= ⬤ F
       * [6] F ::= ⬤ '(' E ')'
       * [7] F ::= ⬤ 'id'
       *)

      type 'a parser = token list -> 'a

      fun error ctx [] =
         raise Fail ("Error during parsing at EOF " ^ ctx)
        | error ctx (t :: _) =
         raise Fail ("Error during parsing at " ^ tokenToString t ^ " " ^ ctx)

      (*** Recursive Descent ***)

      (** Rules **)
      (* These match the input grammar, and are executed
       * once the recognition point is reached *)

      (* E* ::= ⬤ E *)
      fun ruleE k = parseE k

      (* E ::= E '*' ⬤ T *)
      and ruleEMulT k = parseT k

      (* E ::= E '*' ⬤ '*' T *)
      and ruleEExpT k = parseStar (fn () => parseT k)

      (* E ::= ⬤ T *)
      and ruleT k = parseT k

      (* T ::= T ⬤ '+' F *)
      and ruleTPlusF k = parsePlus (fn () => parseF k)

      (* T ::= ⬤ F *)
      and ruleF k = parseF k

      (* F ::= ⬤ '(' E ')' *)
      and ruleParenE k =
         parseLParen (fn () =>
         parseE (fn e =>
         parseRParen (fn () =>
         k e)))

      (* F ::= ⬤ 'num' *)
      and ruleNum k = parseNum k

      (** Parsers **)
      (* These are entrypoints into the LR part *)

      and parseE k = stateEEntry k
      and parseT k = stateTEntry k
      and parseF k = stateFEntry k

      (* '*' *)
      and parseStar k (STAR :: ts) = k () ts
        | parseStar _ ts = error "expected '*'" ts

      (* '+' *)
      and parsePlus k (PLUS :: ts) = k () ts
        | parsePlus _ ts = error "expected '+'" ts

      (* '(' *)
      and parseLParen k (LPAREN :: ts) = k () ts
        | parseLParen _ ts = error "expected '('" ts

      (* ')' *)
      and parseRParen k (RPAREN :: ts) = k () ts
        | parseRParen _ ts = error "expected ')'" ts

      (* 'num' *)
      and parseNum k (NUM n :: ts) = k n ts
        | parseNum _ ts = error "expected 'num'" ts

      (*** Recursive Ascent ***)

      (* [_ → · E] *)
      and stateEEntry k =
         let
            fun go e = stateEExit
                  (k e)
                  (fn t => go (Mul (e, t)))
                  (fn t => go (Exp (e, t)))
         in
            ruleT (fn t => go (Term t))
         end

      (* [_ → E ·], [E → E · '*' T], [E → E · '*' '*' T] *)
      and stateEExit onE onEMulT onEExpT ts =
         case ts of
            STAR :: ts' => stateEStar onEMulT onEExpT ts'
          | _ => onE ts

      (* [_ → E '*' · T], [_ → E '*' · '*' T] *)
      and stateEStar onEMulT onEExpT ts =
         case ts of
            STAR :: _ => ruleEExpT onEExpT ts
          | _ => ruleEMulT onEMulT ts

      (* [_ → · T] *)
      and stateTEntry k =
         let
            fun go t = stateTExit
                  (fn ts => k t ts)
                  (fn f => fn ts => go (Plus (t, f)) ts)
         in
            ruleF (fn f => go (Fact f))
         end

      (* [_ → T ·], [T → T · '+' F] *)
      and stateTExit onT onTPlusF ts =
         case ts of
            PLUS :: _ => ruleTPlusF onTPlusF ts
          | _ => onT ts

      (* [_ → · F] *)
      and stateFEntry k ts =
         let
            fun go e = stateFExit
                  (k e)
         in
            case ts of
               NUM _ :: _ => ruleNum (fn n => go (Num n)) ts
             | LPAREN :: _ => ruleParenE (fn e => go (Paren e)) ts
             | _ => error "expected Factor" ts
         end

      (* [_ → F ·] *)
      and stateFExit k ts = k ts

      val _: (exp -> 'a parser) -> 'a parser = ruleE
      val _: (term -> 'a parser) -> 'a parser = ruleEMulT
      val _: (term -> 'a parser) -> 'a parser = ruleEExpT
      val _: (term -> 'a parser) -> 'a parser = ruleT
      val _: (factor -> 'a parser) -> 'a parser = ruleTPlusF
      val _: (factor -> 'a parser) -> 'a parser = ruleF
      val _: (exp -> 'a parser) -> 'a parser = ruleParenE
      val _: (int -> 'a parser) -> 'a parser = ruleNum

      val _: (unit -> 'a parser) -> 'a parser = parseLParen
      val _: (unit -> 'a parser) -> 'a parser = parseRParen
      val _: (unit -> 'a parser) -> 'a parser = parsePlus
      val _: (unit -> 'a parser) -> 'a parser = parseStar
      val _: (int -> 'a parser) -> 'a parser = parseNum
      val _: (exp -> 'a parser) -> 'a parser = parseE
      val _: (term -> 'a parser) -> 'a parser = parseT
      val _: (factor -> 'a parser) -> 'a parser = parseF


      val entry =
         ruleE (fn e =>
            fn [] => e
             | ts => error "expected EOF" ts)

      val _: token list -> exp = entry
   end

structure G' =
   struct
      open G

      fun parseE k =
         let
            fun go e (STAR :: STAR :: ts) = parseT (fn t => go (Exp (e, t))) ts
              | go e (STAR :: ts) = parseT (fn t => go (Mul (e, t))) ts
              | go e ts = k e ts
         in
            parseT (fn t => go (Term t))
         end

      and parseT k =
         let
            fun go t (PLUS :: ts) = parseF (fn f => go (Plus (t, f))) ts
              | go t ts = k t ts
         in
            parseF (fn f => go (Fact f))
         end

      and parseF k (NUM n :: ts) = k (Num n) ts
        | parseF k (LPAREN :: ts) =
         parseE (fn e =>
         parseRParen (fn () =>
         k (Paren e))) ts
        | parseF _ _ = raise Fail "Expected Factor"

      and parseRParen k (RPAREN :: ts) = k () ts
        | parseRParen _ _ = raise Fail "Expected ')'"

      val entry =
         parseE (fn e =>
            fn [] => (print "foobar\n"; e)
             | ts => raise Fail "Expected EOF")
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
