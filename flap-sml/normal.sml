
functor NormalFn(T : ORD_KEY) =
struct

structure TermMap = RedBlackMapFn(T)

datatype 'a prod = Prod of {
   nonterms : exn list,
   semact : string * exn list -> 'a
}

datatype 'a rhs = Rhs of 'a prod TermMap.map * 'a option

end

structure CFG =
struct

   datatype 'term item = Term of 'term | Nonterm of int

   datatype 'term rule = Rule of 'term item list

   datatype 'term prod = Prod of 'term rule list

   datatype 'term grammar = Grammar of {
         start : 'term prod,
         prods : 'term prod vector
      }

   fun itemToString _ (Nonterm n) = "A" ^ Int.toString n
     | itemToString tos (Term t) = tos t

   fun ruleToString tos (Rule items) =
      String.concatWith " " (List.map (itemToString tos) items)

   fun prodToString tos name (Prod rules) =
      String.concatWith "\n" (List.map (fn rule => name ^ " -> " ^ ruleToString tos rule) rules)

   fun toString tos (Grammar {start, prods}) =
      String.concatWith "\n"
         (prodToString tos "S" start ::
         Vector.foldri
            (fn (i, prod, acc) =>
               prodToString tos ("A" ^ Int.toString i) prod :: acc)
            []
            prods)

   fun print tos g = TextIO.print (toString tos g ^ "\n")

   val G1 : char grammar = Grammar {
         start = Prod [Rule [Term #"a"],
                       Rule [Nonterm 0, Nonterm 1]],
         prods = Vector.fromList [
            Prod [Rule [Term #"a"]],
            Prod [Rule [Term #"z"]]
         ]
      }

   val G2 : char grammar = Grammar {
         start = Prod [Rule [Term #"a"],
                       Rule [Term #"a", Nonterm 0]],
         prods = Vector.fromList [
            Prod [Rule [Term #"a"]]
         ]
      }


   (* fun toCNF (Grammar {start, prods}) = *)
   (*    let *)
   (*    in *)
   (*    end *)
end

structure CNF =
struct

   datatype 'term rule
      = RuleTerm of 'term
      | RuleTwo of nonterm * nonterm

   withtype nonterm = int

   datatype 'term prod = Prod of 'term prod list

   datatype 'term grammar = Grammar of {
         start : 'term prod,
         epsilon : bool,
         prods : 'term prod vector
      }

   (* fun startToString tos start epsilon = *)
   (*    ruleToString tos *) 

   (* fun toString tos (Grammar {start, epsilon, prods}) = *)
   (*    String.concatWith "\n" *)
   (*       (startToString tos start epsilon :: *)
   (*       Vector.foldri *)
   (*          (fn (i, prod, acc) => *)
   (*             prodToString tos ("A" ^ Int.toString i) prod :: acc) *)
   (*          [] *)
   (*          prods) *)

end

structure GNF =
struct

   datatype 'term prod = Prod of 'term * nonterm list

   withtype nonterm = int

   datatype 'term grammar = Grammar of {
         prods : 'term prod vector
      }

end

(* vim: set tw=0 sw=3 ts=3: *)
