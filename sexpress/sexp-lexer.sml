
structure SExpLexer =
struct
   datatype token =
      ERROR of string
    | COMMENT                   (* #; *)
    | STRING of string          (* "" *)
    | QUOTE                     (* ' *)
    | CHARACTER of Word32.word  (* #\newline *)
    | SYMBOL of string          (* foo-bar *)
    | NUMBER of SExp.Num.t      (* 3.14+2/3i *)
    | BOOLEAN of bool           (* #t *)
    | LPAREN                    (* ( *)
    | RPAREN                    (* ) *)
    | LVECTOR                   (* #( *)
    | LBVECTOR                  (* #u8( *)
    | LABELDEF                  (* #1= *)
    | LABELREF                  (* #1# *)
    | EOF

   datatype tok = Tok of {
      token: token,
      line: int,
      column: int
   }
end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
