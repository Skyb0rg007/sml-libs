functor Id( ):
sig
   type t

   val new: string -> t
   val toString: t -> string
   val same: t * t -> bool
   val compare: t * t -> order
   structure Set: ORD_SET where type Key.ord_key = t
   structure Map: ORD_MAP where type Key.ord_key = t
end =
struct
   datatype t = T of string * int

   val counter = ref 0

   fun new s = T (s, !counter before counter := !counter + 1)

   fun toString (T (s, _)) = s

   fun same (T (_, a), T (_, b)) = a = b

   fun compare (T (_, a), T (_, b)) = Int.compare (a, b)

   structure Key =
      struct
         type ord_key = t
         val compare = compare
      end

   structure Set = RedBlackSetFn(Key)
   structure Map = RedBlackMapFn(Key)
end

structure CPS =
struct
   structure Var = Id( )

   structure Literal =
   struct
      datatype t =
         INT of int
       | BOOL of bool
       | CHAR of Word32.word

      fun toString (INT n) =
         if n < 0
            then "-" ^ Int.toString (~n)
            else Int.toString n
        | toString (BOOL b) = if b then "#t" else "#f"
        | toString (CHAR c) = "#\\x" ^ Word32.fmt StringCvt.HEX c
   end

   structure Atom =
   struct
      datatype t =
         VAR of Var.t
       | LIT of Literal.t

      fun toString (VAR v) = Var.toString v
        | toString (LIT l) = Literal.toString l
   end

   structure Exp =
   struct
      datatype t =
         LETFUN of lambda list * t
       | LETATOM of Var.t * Atom.t * t
       | APPLY of Var.t * Var.t list
       | IF of Var.t * t * t

      withtype lambda = { name: Var.t, args: Var.t list, body: t }

      local
         fun indent d = CharVector.tabulate (d+1, fn 0 => #"\n" | _ => #" ")

         fun collectAtoms (LETATOM (x, a, e)) =
            (case collectAtoms e of
                (binds, e) => ((x, a) :: binds, e))
           | collectAtoms e = ([], e)

         fun toString' d (LETFUN (funs, e)) =
            let
               val head = if List.length funs = 1 then "let" else "letrec"
               val d' = d + 3 + String.size head
               fun go {name, args, body} =
                  let
                     val name = Var.toString name
                     val d'' = d' + 4 + String.size name
                  in
                     "(" ^ name ^ " "
                     ^ "(lambda (" ^ String.concatWith " " (List.map Var.toString args) ^ ")"
                     ^ indent d'' ^ toString' d'' body
                     ^ "))"
                  end
            in
               "(" ^ head ^ " (" ^ String.concatWith (indent d') (List.map go funs) ^ ")"
               ^ indent (d + 2) ^ toString' (d + 2) e ^ ")"
            end
           | toString' d (e as LETATOM _) =
            let
               val (binds, e) = collectAtoms e
               fun go (x, a) = "(" ^ Var.toString x ^ " " ^ Atom.toString a ^ ")"
            in
               "(let (" ^ String.concatWith (indent (d + 6)) (List.map go binds) ^ ")"
               ^ indent (d + 2) ^ toString' (d + 2) e ^ ")"
            end
           | toString' d (APPLY (f, xs)) =
            "(" ^ String.concatWith " " (List.map Var.toString (f :: xs)) ^ ")"
           | toString' d (IF (cond, e1, e2)) =
            "(if " ^ Var.toString cond
            ^ indent (d + 2) ^ toString' (d + 2) e1
            ^ indent (d + 2) ^ toString' (d + 2) e2 ^ ")"
      in
         val toString = toString' 0
      end
   end

   structure ExpF =
   struct
      datatype 'a t =
         LETFUN of 'a lambda list * 'a
       | LETATOM of Var.t * Atom.t * 'a
       | APPLY of Var.t * Var.t list
       | IF of Var.t * 'a * 'a

      withtype 'a lambda = { name: Var.t, args: Var.t list, body: 'a }

      fun project (Exp.LETFUN args) = LETFUN args
        | project (Exp.LETATOM args) = LETATOM args
        | project (Exp.APPLY args) = APPLY args
        | project (Exp.IF args) = IF args

      fun embed (LETFUN args) = Exp.LETFUN args
        | embed (LETATOM args) = Exp.LETATOM args
        | embed (APPLY args) = Exp.APPLY args
        | embed (IF args) = Exp.IF args

      fun map f (LETFUN (funs, e)) = LETFUN (List.map (mapLam f) funs, f e)
        | map f (LETATOM (x, a, e)) = LETATOM (x, a, f e)
        | map _ (APPLY (x, xs)) = APPLY (x, xs)
        | map f (IF (cond, e1, e2)) = IF (cond, f e1, f e2)

      and mapLam f {name, args, body} = {name = name, args = args, body = f body}

      fun foldl f z (LETFUN (funs, e)) = f (e, List.foldl (fn (a, b) => f (#body a, b)) z funs)
        | foldl f z (LETATOM (_, _, e)) = f (e, z)
        | foldl _ z (APPLY _) = z
        | foldl f z (IF (_, e1, e2)) = f (e2, f (e1, z))

      fun foldr f z (LETFUN (funs, e)) = List.foldr (fn (a, b) => f (#body a, b)) (f (e, z)) funs
        | foldr f z (LETATOM (_, _, e)) = f (e, z)
        | foldr _ z (APPLY _) = z
        | foldr f z (IF (_, e1, e2)) = f (e1, f (e2, z))

      fun mapAccumL f s (LETFUN (funs, e)) =
         let
            fun go (s, []) = (s, [])
              | go (s, {name, args, body} :: xs) =
               let
                  val (s, body) = f (s, body)
                  val (s, xs) = go (s, xs)
               in
                  (s, {name = name, args = args, body = body} :: xs)
               end

            val (s, funs) = go (s, funs)
            val (s, e) = f (s, e)
         in
            (s, LETFUN (funs, e))
         end
        | mapAccumL f s (LETATOM (x, a, e)) =
         let
            val (s, e) = f (s, e)
         in
            (s, LETATOM (x, a, e))
         end
        | mapAccumL _ s (APPLY (x, xs)) = (s, APPLY (x, xs))
        | mapAccumL f s (IF (cond, e1, e2)) =
         let
            val (s, e1) = f (s, e1)
            val (s, e2) = f (s, e2)
         in
            (s, IF (cond, e1, e2))
         end

      fun cata alg x = alg (map (cata alg) (project x))

      fun para alg x = alg (map (fn x => (x, para alg)) (project x))

      fun ana coalg x = embed (map (ana coalg) (coalg x))

      fun apo coalg x = embed (map (fn Either.INL x => x | Either.INR x => apo coalg x) (coalg x))
   end

   structure ExpZ =
   struct
      datatype path = Top | Path of node ExpF.t
      withtype node = (Exp.t, path) Either.either

      type t = Exp.t * path

      fun root e = (e, Top)

      fun defocus (foc, path) =
         let
            fun go (t, Top) = t
              | go (t, Path xs) =
               let
                  fun h (old, Either.INL y) = (old, y)
                    | h (_, Either.INR p) = (SOME p, t)

                  val (path', s) = ExpF.mapAccumL h NONE xs
               in
                  go (ExpF.embed s, Option.valOf path')
               end
         in
            go (foc, path)
         end

      fun focus (foc, _) = foc

      fun replace (new, (_, path)) = (new, path)

      fun modify f loc = replace (f (focus loc), loc)

      fun moveDown (pos, (foc, path)) =
         let
            fun g ((old, j), x) =
               if j = pos
                  then ((SOME x, j + 1), Either.INR path)
               else ((old, j + 1), Either.INL x)
            val ((mfoc', _), nodes') = ExpF.mapAccumL g (NONE, 0) (ExpF.project foc)
         in
            case mfoc' of
               NONE => NONE
             | SOME foc' => SOME (foc', Path nodes')
         end

      fun moveUp (_, Top) = NONE
        | moveUp (foc, Path nodes) =
         let
            fun g (old, Either.INL x) = (old, x)
              | g (_, Either.INR p) = (SOME p, foc)
            val (mpath, foc') = ExpF.mapAccumL g NONE nodes
         in
            case mpath of
               NONE => raise Fail "CPS.ExpZ.moveUp: Invariant broken"
             | SOME path => SOME (ExpF.embed foc', path)
         end
   end

   val x = Var.new "x"
   val y = Var.new "y"
   val z = Var.new "z"
   val f = Var.new "f"
   val f2 = Var.new "f-two"
   val one = Atom.LIT (Literal.INT 1)
   val two = Atom.LIT (Literal.INT 2)
   val e1 = Exp.LETATOM (x, one, Exp.LETATOM (x, two, Exp.APPLY (x, [z])))
   val e2 = Exp.LETATOM (z, one, Exp.LETATOM (z, two, Exp.APPLY (z, [x])))
   val e3 = Exp.APPLY (f2, [f2])
   val lam1 = { name = f, args = [x, y], body = e1 }
   val lam2 = { name = f2, args = [x, y], body = e2 }
   val e4 = Exp.LETFUN ([lam1, lam2], e3)
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)

