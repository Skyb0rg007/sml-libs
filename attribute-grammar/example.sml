
signature EXP =
sig
   type t

   datatype node =
      Var of string
    | Int of int
    | Bool of bool
    | If of t * t * t
    | Abs of string * t
    | App of t * t
    | Let of string * t * t

   val wrap: node -> t
   val node: t -> node
   val holder: t -> PropList.holder
   val toString: t -> string
end

signature EXPZ =
sig
   type exp
   type t

   exception Invalid

   val fromExp: exp -> t

   val current: t -> exp
   val up: t -> t
   val down: t -> t
   val left: t -> t
   val right: t -> t
   val modify: (exp -> exp) -> t -> t

   val up': t -> t option
   val down': t -> t option
   val left': t -> t option
   val right': t -> t option
end

structure Ex:
sig
   structure Exp: EXP
   structure ExpZ: EXPZ where type exp = Exp.t
   structure Memo:
   sig
      type 'a memo
      val newMemo: unit -> 'a memo
      val memoize: 'a memo * (ExpZ.t -> 'a) -> ExpZ.t -> 'a
   end
end =
struct

   structure Exp =
   struct
      datatype node =
         Var of string
       | Int of int
       | Bool of bool
       | If of t * t * t
       | Abs of string * t
       | App of t * t
       | Let of string * t * t
      withtype t = PropList.holder * node

      fun wrap e = (PropList.newHolder (), e)

      fun node (_, e) = e

      fun holder (h, _) = h

      fun toString' (Var v) = v
        | toString' (Int n) = if n >= 0 then Int.toString n else "-" ^ Int.toString (~n)
        | toString' (Bool b) = if b then "#t" else "#f"
        | toString' (If (e1, e2, e3)) = String.concat ["(if ", toString e1, " ", toString e2, " ", toString e3]
        | toString' (Abs (x, e)) = String.concat ["(lambda (", x, ") ", toString e, ")"]
        | toString' (App (f, x)) = String.concat ["(", toString f, " ", toString x, ")"]
        | toString' (Let (x, e1, e2)) = String.concat ["(let ((", x, " ", toString e1, ")) ", toString e2, ")"]
      and toString e = toString' (node e)
   end

   structure ExpZ =
   struct
      type exp = Exp.t

      (* Derivative of exp *)
      datatype dt =
         If1 of PropList.holder * Exp.t * Exp.t
       | If2 of PropList.holder * Exp.t * Exp.t
       | If3 of PropList.holder * Exp.t * Exp.t
       | Abs of PropList.holder * string
       | App1 of PropList.holder * Exp.t
       | App2 of PropList.holder * Exp.t
       | Let1 of PropList.holder * string * Exp.t
       | Let2 of PropList.holder * string * Exp.t

      (* Zipper type *)
      datatype t = T of dt list * Exp.t

      exception Invalid

      fun fromExp e = T ([], e)

      fun current (T (_, e)) = e

      fun modify f (T (cs, e)) = T (cs, f e)

      fun up' (T (If1 (h, e2, e3) :: cs, e1)) = SOME (T (cs, (h, Exp.If (e1, e2, e3))))
        | up' (T (If2 (h, e1, e3) :: cs, e2)) = SOME (T (cs, (h, Exp.If (e1, e2, e3))))
        | up' (T (If3 (h, e1, e2) :: cs, e3)) = SOME (T (cs, (h, Exp.If (e1, e2, e3))))
        | up' (T (Abs (h, x) :: cs, e)) = SOME (T (cs, (h, Exp.Abs (x, e))))
        | up' (T (App1 (h, e2) :: cs, e1)) = SOME (T (cs, (h, Exp.App (e1, e2))))
        | up' (T (App2 (h, e1) :: cs, e2)) = SOME (T (cs, (h, Exp.App (e1, e2))))
        | up' (T (Let1 (h, x, e2) :: cs, e1)) = SOME (T (cs, (h, Exp.Let (x, e1, e2))))
        | up' (T (Let2 (h, x, e1) :: cs, e2)) = SOME (T (cs, (h, Exp.Let (x, e1, e2))))
        | up' _ = NONE

      fun down' (T (cs, (h, Exp.If (e1, e2, e3)))) = SOME (T (If1 (h, e2, e3) :: cs, e1))
        | down' (T (cs, (h, Exp.Abs (x, e)))) = SOME (T (Abs (h, x) :: cs, e))
        | down' (T (cs, (h, Exp.App (e1, e2)))) = SOME (T (App1 (h, e2) :: cs, e1))
        | down' (T (cs, (h, Exp.Let (x, e1, e2)))) = SOME (T (Let1 (h, x, e2) :: cs, e1))
        | down' _ = NONE

      fun left' (T (If2 (h, e1, e3) :: cs, e2)) = SOME (T (If1 (h, e2, e3) :: cs, e1))
        | left' (T (If3 (h, e1, e2) :: cs, e3)) = SOME (T (If2 (h, e1, e3) :: cs, e2))
        | left' (T (App2 (h, e1) :: cs, e2)) = SOME (T (App1 (h, e2) :: cs, e1))
        | left' (T (Let2 (h, x, e1) :: cs, e2)) = SOME (T (Let1 (h, x, e2) :: cs, e1))
        | left' _ = NONE

      fun right' (T (If1 (h, e2, e3) :: cs, e1)) = SOME (T (If2 (h, e1, e3) :: cs, e2))
        | right' (T (If2 (h, e1, e3) :: cs, e2)) = SOME (T (If3 (h, e1, e2) :: cs, e3))
        | right' (T (App1 (h, e2) :: cs, e1)) = SOME (T (App2 (h, e1) :: cs, e2))
        | right' (T (Let1 (h, x, e2) :: cs, e1)) = SOME (T (Let2 (h, x, e1) :: cs, e2))
        | right' _ = NONE

      fun valOf NONE = raise Invalid
        | valOf (SOME x) = x

      val up = valOf o up'
      val down = valOf o down'
      val left = valOf o left'
      val right = valOf o right'
   end

   structure Memo:
   sig
      type 'a memo

      val newMemo: unit -> 'a memo
      val memoize: 'a memo * (ExpZ.t -> 'a) -> ExpZ.t -> 'a
   end =
   struct
      type 'a memo = {
         get: ExpZ.t -> 'a option,
         set: ExpZ.t * 'a -> unit
      }

      fun newMemo () =
         let
            fun err _ = raise Fail "Accessed prop before it's set!"
            val {peekFn, setFn, ...} = PropList.newProp (Exp.holder o ExpZ.current, err)
         in
            {get = peekFn, set = setFn}
         end

      fun memoize ({get, set}, f) =
         let
            fun f' z =
               case get z of
                  SOME x => x
                | NONE =>
                     let
                        val x = f z
                     in
                        set (z, x)
                        ; x
                     end
         in
            f'
         end
   end
end

(* vim: set tw=0 sw=3 ts=3: *)
