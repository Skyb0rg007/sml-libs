
structure MiniKanren :
sig
  datatype term
    = Var of int
    | Compound of Atom.atom * term list

  val termToString : term -> string

  val atom : string -> term
  val compound : string * term list -> term

  type goal

  val fresh : (term -> goal) -> goal
  val == : term * term -> goal
  val conj : goal * goal -> goal
  val disj : goal * goal -> goal

  val run : int * goal -> (int * term) list list
end =
struct
  datatype term
    = Var of int
    | Compound of atom * term list

  withtype atom = Atom.atom

  fun termToString (Var n) = "X" ^ Int.toString n
    | termToString (Compound (a, [])) = Atom.toString a
    | termToString (Compound (f, args)) =
      Atom.toString f ^ "(" ^ String.concatWithMap ", " termToString args ^ ")"

  datatype 'a node = Nil | Cons of 'a * 'a seq

  withtype 'a seq = unit -> 'a node

  type env = (int * term) list
  type state = env * int
  type goal = state -> state seq

  fun compound (f, args) = Compound (Atom.atom f, args)
  fun atom a = compound (a, [])

  fun walk (env : env) t =
    case t of
        Compound _ => t
      | Var u =>
        case List.find (fn (v, _) => u = v) env of
            NONE => t
          | SOME (_, t') => t'

  fun unify env (u, v) =
    case (walk env u, walk env v) of
        (Var x, Var y) =>
          if x = y
            then SOME env
          else SOME ((x, Var y) :: env)
      | (Var x, v) => SOME ((x, v) :: env)
      | (u, Var y) => SOME ((y, u) :: env)
      | (Compound (f, us), Compound (g, vs)) =>
          if Atom.same (f, g) andalso List.length us = List.length vs
            then unifyAll env (us, vs)
          else NONE

  and unifyAll env ([], _) = SOME env
    | unifyAll env (_, []) = SOME env
    | unifyAll env (u :: us, v :: vs) =
    case unify env (u, v) of
        NONE => NONE
      | SOME env' => unifyAll env' (us, vs)

  fun empty () = Nil

  fun singleton x () = Cons (x, empty)

  fun interleave (seq1, seq2) () =
    case seq1 () of
        Nil => seq2 ()
      | Cons (x, xs) => Cons (x, interleave (seq2, xs))

  fun concatMap f seq () =
    case seq () of
        Nil => Nil
      | Cons (x, xs) => interleave (f x, concatMap f xs) ()

  fun == (u : term, v : term) : goal =
    fn (env, c) =>
      case unify env (u, v) of
          NONE => empty
        | SOME env' => singleton (env', c)

  fun disj (g1 : goal, g2 : goal) : goal =
    fn s => interleave (g1 s, g2 s)

  fun conj (g1 : goal, g2 : goal) : goal =
    fn s => concatMap g2 (g1 s)

  fun fresh (f : term -> goal) : goal =
    fn (env, c) => f (Var c) (env, c + 1)

  fun run (n : int, g : goal) =
    let
      fun take (0, _, acc) = List.rev acc
        | take (n, seq, acc) =
          case seq () of
              Nil => List.rev acc
            | Cons ((x, _), xs) => take (n - 1, xs, x :: acc)
    in
      take (n, g ([], 0), [])
    end
end
