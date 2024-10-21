
functor LexerFn(type alphabet) :
sig

type regex

val empty: regex
val elem: (alphabet -> bool) -> regex
val union: regex * regex -> regex
val cat: regex * regex -> regex
val star: regex -> regex
val epsilon: regex

end =
struct

datatype re
   = Epsilon
   | Any
   | None
   | SymSet of alphabet -> bool
   | Concat of re list
   | Closure of re
   | Not of re
   | And of re list
   | Or of re list

datatype regex = Empty
               | Epsilon
               | Elem of alphabet -> bool
               (* ∅∪L = L∪∅ = L
                * so Union (L₁, L₂) ⇔ L₁ ≠ ∅ ∧ L₂ ≠ ∅
                * therefore (nullable ∨ hasNonempty) *)
               | Union of regex * regex * {nullable: bool, hasNonempty: bool}
               (* ∅⋅L = L⋅∅ = ∅
                * {ε}⋅L = L⋅{ε} = L
                * so Concat (L₁, L₂) ⇔ L₁ ≠ ∅ ∧ L₂ ≠ ∅ ∧ L₁ ≠ {ε} ∧ L₂ ≠ {ε} *)
               | Concat of regex * regex * {nullable: bool}
               (* ∅* = {ε}
                * {ε}* = {ε}
                * L** = L*
                * so Star L ⇔ L ≠ ∅ ∧ L ≠ {ε}
                *)
               | Star of regex

fun nullable Empty = false
  | nullable Epsilon = true
  | nullable (Elem _) = false
  | nullable (Union (_, _, {nullable, ...})) = nullable
  | nullable (Concat (_, _, {nullable, ...})) = nullable
  | nullable (Star _) = true

fun hasNonempty Empty = false
  | hasNonempty Epsilon = false
  | hasNonempty (Elem _) = true
  | hasNonempty (Union (_, _, {hasNonempty, ...})) = hasNonempty
  | hasNonempty (Concat _) = true
  | hasNonempty (Star _) = true

val empty = Empty
val epsilon = Epsilon
val elem = Elem
fun union (Empty, r2) = r2
  | union (r1, Empty) = r1
  | union (r1, r2) = Union (r1, r2, {nullable = nullable r1 orelse nullable r2, hasNonempty = hasNonempty r1 orelse hasNonempty r2})
fun cat (Empty, _) = Empty
  | cat (_, Empty) = Empty
  | cat (Epsilon, r2) = r2
  | cat (r1, Epsilon) = r1
  | cat (r1, r2) = Concat (r1, r2, {nullable = nullable r1 andalso nullable r2})
fun star Empty = Epsilon
  | star Epsilon = Epsilon
  | star (Star r) = Star r
  | star r = Star r

end

structure Lexer = LexerFn(type alphabet = char)

(* vim: set tw=0 ts=3 sw=3: *)
