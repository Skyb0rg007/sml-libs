
structure Lalr =
struct
  structure NameSet = Grammar.NameSet
  datatype priority = datatype Grammar.priority
  datatype assoc = datatype Grammar.assoc

  (* rule number, dot position *)
  datatype lr0_item = Lr0 of int * int

  (* rule number, dot position, lookahead *)
  datatype lr1_item = Lr1 of int * int * NameSet.set

  type rule_list = lr0_item list

  datatype lr_action
    = Shift of int * priority
    | Reduce of int * priority
    | Accept
    | Fail
    | MustFail
    | Conflict of lr_action list * lr_action

  datatype goto = Goto of int | NoGoto

  structure ActionTable =
    struct
      type t = lr_action Array2.array

      fun sub (tbl, s, Grammar.T t) = Array2.sub (tbl, s, t)
      fun update (tbl, s, Grammar.T t, x) = Array2.update (tbl, s, t, x)
    end

  structure GotoTable =
    struct
      type t = goto Array2.array

      fun sub (tbl, s, Grammar.NT t) = Array2.sub (tbl, s, t)
      fun update (tbl, s, Grammar.NT t, x) = Array2.update (tbl, s, t, x)
    end

end
