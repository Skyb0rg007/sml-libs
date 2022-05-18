
structure Typeof =
struct
   open Ex

   structure StrMap = RedBlackMapFn(
      struct
         type ord_key = string
         val compare = String.compare
      end)

   structure Type =
   struct
      datatype ty =
         Int
       | Bool
       | Arrow of ty * ty
       | Var of string

      datatype scheme = Forall of string list * ty

      fun tyString Int = "int"
        | tyString Bool = "bool"
        | tyString (Arrow (a, b)) = "(" ^ tyString a ^ " -> " ^ tyString b ^ ")"
        | tyString (Var x) = x

      fun schemeString (Forall (xs, t)) =
         "(forall (" ^ String.concatWith " " xs ^ ") " ^ tyString t ^ ")"
   end

   type ctx = (string list, Type.scheme) Either.either StrMap.map

   datatype ty =
      Int
    | Bool
    | Arrow of ty * ty
    | Error of string list
   type ctx = ty AtomMap.map

   fun tyString Int = "int"
     | tyString Bool = "bool"
     | tyString (Arrow (a, b)) = "(" ^ tyString a ^ " -> " ^ tyString b ^ ")"
     | tyString (Error es) = "(Error [" ^ String.concatWith ", " es ^ "])"

   val typeofMemo: ty Memo.memo = Memo.newMemo ()
   val contextMemo: ctx Memo.memo = Memo.newMemo ()

   fun typeof' z =
      case Exp.node (ExpZ.current z) of
         Exp.Var x =>
            let
               val ctx = context z
            in
               case AtomMap.find (ctx, Atom.atom x) of
                  NONE => Error ["Unbound variable " ^ x]
                | SOME ty => ty
            end
       | Exp.Int _ => Int
       | Exp.Bool _ => Bool
       | Exp.If _ =>
            let
               val t1 = typeof (ExpZ.down z)
               val t2 = typeof (ExpZ.right (ExpZ.down z))
               val t3 = typeof (ExpZ.right (ExpZ.right (ExpZ.down z)))
               val err1 =
                  if t1 = Bool
                     then NONE
                  else SOME ("Condition must be bool, got " ^ tyString t1)
               val err2 =
                  if t2 = t3
                     then NONE
                  else SOME ("If branches don't match, got " ^ tyString t2 ^ " and " ^ tyString t3)
            in
               case (err1, err2) of
                  (NONE, NONE) => t2
                | (SOME a, SOME b) => Error [a, b]
                | (SOME a, NONE) => Error [a]
                | (NONE, SOME b) => Error [b]
            end
       | Exp.Abs (x, e) =>
            let
               val t = typeof (ExpZ.down z)
            in
               raise Fail "NYI"
            end
       | _ => raise Match

   and context' z =
      let
         val ctx =
            Option.getOpt (Option.map context (ExpZ.up' z), AtomMap.empty)
      in
         case Exp.node (ExpZ.current z) of
            _ => raise Match
      end

   and typeof x = Memo.memoize (typeofMemo, typeof') x
   and context x = Memo.memoize (contextMemo, context') x
end

(* vim: set tw=0 sw=3 ts=3: *)
