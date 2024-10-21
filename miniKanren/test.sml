
structure Test =
struct
  structure MK = MiniKanren

  fun display [] = ()
    | display ((v, term) :: rest) =
      (print (MK.termToString (MK.Var v) ^ " = " ^ MK.termToString term ^ "\n");
       display rest)

  fun test1 () =
    let
      val g =
        MK.fresh (fn q =>
        MK.== (q, MK.atom "5"))
    in
      MK.run (99, g)
    end

  fun test2 () =
    let
      val g =
        MK.conj
          (MK.fresh (fn a => MK.== (a, MK.atom "7")),
           MK.fresh (fn b =>
            MK.disj (MK.== (b, MK.atom "5"), MK.== (b, MK.atom "6"))))
    in
      MK.run (99, g)
    end
end
