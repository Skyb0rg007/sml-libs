
structure Grammar =
struct
  datatype terminal = T of int
  datatype nonterminal = NT of int
  datatype name = TERM of terminal | NONTERM of nonterminal

  local
    structure K =
      struct
        type ord_key = name

        fun compare (TERM (T n), TERM (T m)) = Int.compare (n, m)
          | compare (NONTERM (NT n), NONTERM (NT m)) = Int.compare (n, m)
          | compare (TERM _, NONTERM _) = LESS
          | compare (NONTERM _, TERM _) = GREATER
      end
  in
    structure NameSet = RedBlackSetFn(K)
    structure NameMap = RedBlackMapFn(K)
  end

  datatype assoc
    = LeftAssoc
    | RightAssoc
    | NonAssoc

  datatype priority
    = No
    | Prio of assoc * int
    | PrioLowest

  datatype grammar = Grammar of {
      rules : rule list,
      terminals : terminal list,
      nonterminals : nonterminal list,
      token_names : name -> string,
      start : nonterminal,
      eof : terminal
    }

  withtype rule = {
      lhs : nonterminal,
      rhs : name list,
      priority : priority,
      rp : int (* Recognition point *)
    }

  fun precalcClosure0 (Grammar g) =
    let
      fun followNT _ ({rhs = [], ...} : rule) = NameSet.empty
        | followNT f ({rhs = nt :: _, ...}) = NameMap.lookup (f, nt)

      (* fun follow f rules = *)
      (*   let *)
      (*     fun newRules (rule, rs) = *)
      (*       NameSet.union (rs, followNT f rule) *)
      (*   in *)
      (*     NameSet.foldl newRules rules rules *)
      (*   end *)
    in
      ()
    end
end
