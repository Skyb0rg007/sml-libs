
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

structure Exp =
struct
   structure Var = Id( )

   datatype t =
      Var of Var.t
    | Int of int
    | Plus of t * t
    | Bool of bool
    | App of t * t
    | Abs of Var.t * t
    | Let of Var.t * t * t

   fun toString (Var v) = Var.toString v
     | toString (Int n) = String.map (fn #"~" => #"-" | c => c) (Int.toString n)
     | toString (Plus (a, b)) = "(+ " ^ toString a ^ " " ^ toString b ^ ")"
     | toString (Bool b) = if b then "#t" else "#f"
     | toString (App (a, b)) = "(" ^ toString a ^ " " ^ toString b ^ ")"
     | toString (Abs (x, e)) = "(lambda (" ^ Var.toString x ^ ") " ^ toString e ^ ")"
     | toString (Let (x, e1, e2)) = "(let ((" ^ Var.toString x ^ " " ^ toString e1 ^ ")) " ^ toString e2 ^ ")"

   val x = Var.new "x"
   val y = Var.new "y"
   val z = Var.new "z"
   val e = Let (x, Abs (z, Int 4), Let (y, Int 5, Plus (Var x, Var y)))
end

structure Type =
struct
   structure Var = Id( )

   datatype t =
      Var of Var.t
    | Int
    | Bool
    | Arrow of t * t

   datatype scheme = Forall of Var.Set.set * t
end

structure ExpF =
struct
   structure Var = Exp.Var

   datatype 'a expF =
      Var of Var.t
    | Int of int
    | Plus of 'a * 'a
    | Bool of bool
    | App of 'a * 'a
    | Abs of Var.t * 'a
    | Let of Var.t * 'a * 'a

   fun map _ (Var a) = Var a
     | map _ (Int a) = Int a
     | map f (Plus (a, b)) = Plus (f a, f b)
     | map _ (Bool a) = Bool a
     | map f (App (a, b)) = App (f a, f b)
     | map f (Abs (a, b)) = Abs (a, f b)
     | map f (Let (a, b, c)) = Let (a, f b, f c)

   fun embed (Var a) = Exp.Var a
     | embed (Int a) = Exp.Int a
     | embed (Plus (a, b)) = Exp.Plus (a, b)
     | embed (Bool a) = Exp.Bool a
     | embed (App (a, b)) = Exp.App (a, b)
     | embed (Abs (a, b)) = Exp.Abs (a, b)
     | embed (Let (a, b, c)) = Exp.Let (a, b, c)

   fun project (Exp.Var a) = Var a
     | project (Exp.Int a) = Int a
     | project (Exp.Plus (a, b)) = Plus (a, b)
     | project (Exp.Bool a) = Bool a
     | project (Exp.App (a, b)) = App (a, b)
     | project (Exp.Abs (a, b)) = Abs (a, b)
     | project (Exp.Let (a, b, c)) = Let (a, b, c)

   structure Fix:
   sig
      datatype fix = T of fix expF
   end =
   struct
      datatype fix = T of fix expF
   end

   structure Free:
   sig
      datatype 'a free = T of ('a, 'a free expF) Either.either

      val pure: 'a -> 'a free
      val bind: ('a -> 'b free) -> 'a free -> 'b free
      val map: ('a -> 'b) -> 'a free -> 'b free
      val join: 'a free free -> 'a free
   end =
   struct
      datatype 'a free = T of ('a, 'a free expF) Either.either

      fun pure x = T (Either.INL x)

      fun bind f (T (Either.INL x)) = f x
        | bind f (T (Either.INR x)) = T (Either.INR (map (bind f) x))

      fun join x = bind Fn.id x

      fun map' f (T (Either.INL x)) = T (Either.INL (f x))
        | map' f (T (Either.INR x)) = T (Either.INR (map (map' f) x))
      val map = map'
   end

   structure Cofree:
   sig
      datatype 'a cofree = T of 'a * 'a cofree expF

      val unwrap: 'a cofree -> 'a cofree expF
      val extract: 'a cofree -> 'a
      val extend: ('a cofree -> 'b) -> 'a cofree -> 'b cofree
      val map: ('a -> 'b) -> 'a cofree -> 'b cofree
      val duplicate: 'a cofree -> 'a cofree cofree
   end =
   struct
      datatype 'a cofree = T of 'a * 'a cofree expF

      fun unwrap (T (_, x)) = x

      fun extract (T (x, _)) = x

      fun extend f (T (x, a)) = T (f (T (x, a)), map (extend f) a)

      fun duplicate (T (x, y)) = T (T (x, y), map duplicate y)

      fun map' f (T (a, b)) = T (f a, map (map' f) b)
      val map = map'
   end

   fun cata alg x = alg (map (cata alg) (project x))
   fun cata alg x = alg (map (cata alg) (project x))

   fun para alg x = alg (map (fn x => (x, para alg x)) (project x))

   fun ana coalg x = embed (map (ana coalg) (coalg x))

   fun apo coalg x = embed (map (fn Either.INL x => x | Either.INR x => apo coalg x) (coalg x))

   fun distHisto x = Cofree.T (map Cofree.extract x, map (distHisto o Cofree.unwrap) x)
   val _: 'a Cofree.cofree expF -> 'a expF Cofree.cofree = distHisto

   fun distFutu (Free.T (Either.INL x)) = map (Free.T o Either.INL) x
     | distFutu (Free.T (Either.INR x)) = map (Free.T o Either.INR o distFutu) x
   val _: 'a expF Free.free -> 'a Free.free expF = distFutu

   fun synthesize alg x = distHisto (map (Cofree.duplicate o Cofree.map alg o synthesize alg) (project x))

   fun inherit coalg x = embed (map (inherit coalg o Free.map coalg o Free.join) (distFutu x))

   fun histo alg = alg o Cofree.extract o synthesize alg

   fun futu coalg = inherit coalg o Free.pure o coalg
end

structure Opt =
struct
   fun constFoldAlg (ExpF.Var v) env =
      (case Exp.Var.Map.find (env, v) of
          NONE => Exp.Var v
        | SOME n => n)
     | constFoldAlg (ExpF.Int n) _ = Exp.Int n
     | constFoldAlg (ExpF.Plus (a, b)) env =
      (case (a env, b env) of
          (Exp.Int n, Exp.Int m) => Exp.Int (n + m)
        | (a, b) => Exp.Plus (a, b))
     | constFoldAlg (ExpF.Bool b) _ = Exp.Bool b
     | constFoldAlg (ExpF.App (a, b)) env = Exp.App (a env, b env)
     | constFoldAlg (ExpF.Abs (a, b)) env = Exp.Abs (a, b env)
     | constFoldAlg (ExpF.Let (x, a, b)) env =
      (case a env of
          Exp.Int n => b (Exp.Var.Map.insert (env, x, Exp.Int n))
        | a => Exp.Let (x, a, b env))

   fun constFold e = ExpF.cata constFoldAlg e Exp.Var.Map.empty

   fun usesAlg (ExpF.Var v) = Exp.Var.Map.singleton (v, 1)
     | usesAlg (ExpF.Int _) = Exp.Var.Map.empty
     | usesAlg (ExpF.Bool _) = Exp.Var.Map.empty
     | usesAlg (ExpF.Plus (a, b)) = Exp.Var.Map.unionWith op+ (a, b)
     | usesAlg (ExpF.App (a, b)) = Exp.Var.Map.unionWith op+ (a, b)
     | usesAlg (ExpF.Abs (_, b)) = b
     | usesAlg (ExpF.Let (_, a, b)) = Exp.Var.Map.unionWith op+ (a, b)

   (* fun inlineAlg (ExpF.Let (x, a, b)) *) 
end

(* vim: set ts=3 sw=3 tw=0: *)
