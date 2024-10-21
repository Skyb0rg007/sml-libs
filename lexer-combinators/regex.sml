
signature REGEX =
   sig
      structure SymSet : INTERVAL_SET where type D.point = Word8.word

      datatype re = Epsilon
                  | Any
                  | None
                  | SymSet of SymSet.set
                  | Concat of re list
                  | Closure of re
                  | Or of re list
                  | And of re list
                  | Not of re

      val epsilon: re
      val any: re
      val none: re
      val symSet: SymSet.set -> re
      val sym: SymSet.item -> re
      val concat: re * re -> re
      val concatList: re list -> re
      val or: re * re -> re
      val orList: re list -> re
      val and_: re * re -> re
      val andList: re list -> re
      val not: re -> re

      val toString: re -> string
      val nullable: re -> bool
      val quotient: re * re -> re
   end

structure Regex: REGEX =
struct

structure SymSet = IntervalSetFn(
   struct
      type point = Word8.word

      val minPt: point = 0w0
      val maxPt: point = 0w0
      val compare = Word8.compare
      fun succ (w: point) = w + 0w1
      fun pred (w: point) = w - 0w1
      fun isSucc (w1, w2) = succ w1 = w2
   end)

datatype re = Epsilon
            | Any
            | None
            | SymSet of SymSet.set
            | Concat of re list
            | Closure of re
            | Or of re list
            | And of re list
            | Not of re

fun compare (Epsilon, Epsilon) = EQUAL
  | compare (Epsilon, _) = LESS
  | compare (_, Epsilon) = GREATER
  | compare (Any, Any) = EQUAL
  | compare (Any, _) = LESS
  | compare (_, Any) = GREATER
  | compare (None, None) = EQUAL
  | compare (None, _) = LESS
  | compare (_, None) = GREATER
  | compare (SymSet a, SymSet b) = SymSet.compare (a, b)
  | compare (SymSet _, _) = LESS
  | compare (_, SymSet _) = GREATER
  | compare (Concat a, Concat b) = List.collate compare (a, b)
  | compare (Concat _, _) = LESS
  | compare (_, Concat _) = GREATER
  | compare (Closure a, Closure b) = compare (a, b)
  | compare (Closure _, _) = LESS
  | compare (_, Closure _) = GREATER
  | compare (Or a, Or b) = List.collate compare (a, b)
  | compare (Or _, _) = LESS
  | compare (_, Or _) = GREATER
  | compare (And a, And b) = List.collate compare (a, b)
  | compare (And _, _) = LESS
  | compare (_, And _) = GREATER
  | compare (Not a, Not b) = compare (a, b)

val any = Any

val none = None

val epsilon = Epsilon

fun symSet s = 
   if SymSet.isEmpty s
      then None
   else if SymSet.isUniverse s
      then Any
   else SymSet s

fun sym s = symSet (SymSet.singleton s)

fun concat (Epsilon, b) = b
  | concat (a, Epsilon) = a
  | concat (None, _) = None
  | concat (_, None) = None
  | concat (Concat a, Concat b) = Concat (a @ b)
  | concat (a, Concat b) = Concat (a :: b)
  | concat (Concat a, b) = Concat (a @ [b])
  | concat (a, b) = Concat [a, b]

fun concatList [] = Epsilon
  | concatList (r :: rs) = concat (r, concatList rs)

fun closure Epsilon = Epsilon
  | closure None = Epsilon
  | closure (r as Closure _) = r
  | closure r = Closure r

fun mergeSymSets (rs, setOp) =
   let
      fun isSymSet (SymSet _) = true
        | isSymSet _ = false
      val (symSets, rs) = List.partition isSymSet rs
      fun f (SymSet s1, SymSet s2) = SymSet (setOp (s1, s2))
        | f _ = raise Fail "impossible"
   in
      case symSets of
         [] => rs
       | s :: ss => List.foldl f s ss :: rs
   end

local
   fun mk rs =
      case ListMergeSort.uniqueSort compare rs of
         [] => None
       | [r] => r
       | rs => Or rs
in
   fun or (None, b) = b
     | or (a, None) = a
     | or (SymSet s1, SymSet s2) = SymSet (SymSet.union (s1, s2))
     | or (Or a, Or b) = mk (a @ b)
     | or (a, Or b) = mk (a :: b)
     | or (Or a, b) = mk (a @ [b])
     | or (a, b) = mk [a, b]
end

fun orList [] = None
  | orList (r :: rs) = List.foldl or r rs

local
   fun mk rs =
      case ListMergeSort.uniqueSort compare rs of
         [] => Epsilon
       | [r] => r
       | rs => And rs
in
   fun and_ (None, _) = None
     | and_ (_, None) = None
     | and_ (SymSet s1, SymSet s2) = SymSet (SymSet.intersect (s1, s2))
     | and_ (And a, And b) = mk (a @ b)
     | and_ (a, And b) = mk (a :: b)
     | and_ (And a, b) = mk (a @ [b])
     | and_ (a, b) = mk [a, b]
end

fun andList [] = Epsilon
  | andList (r :: rs) = List.foldl and_ r rs

fun not (Not r) = r
  | not None = Closure Any
  | not r = Not r

local
   fun c2s c =
      if c < 0w128
         then Char.toString (Byte.byteToChar c)
      else "\\u" ^ Word8.toString c

   fun range (a, b) =
      if a = b
         then c2s a
      else if a + 0w1 = b
         then String.concat [c2s a, c2s b]
      else String.concat [c2s a, "-", c2s b]

   fun symSetToString s =
      let
         val is = SymSet.intervals s
         val cis = SymSet.intervals (SymSet.complement s)
         val str =
            if List.length is <= List.length cis
               then String.concat (List.map range is)
            else String.concat ("^" :: List.map range cis)
      in
         if String.size str <= 1
            then str
         else String.concat ["[", str, "]"]
      end

   fun paren false s = s
     | paren true s = "(" ^ s ^ ")"

   fun toStringPrec d =
      fn Any => "."
       | Epsilon => ""
       | None => "$^"
       | SymSet s => symSetToString s
       | Concat rs => paren (d > 3) (String.concat (List.map (toStringPrec 0) rs))
       | Closure r => paren (d > 5) (toStringPrec 0 r ^ "*")
       | And rs => paren (d > 2) (String.concatWith "&" (List.map (toStringPrec 0) rs))
       | Or rs => paren (d > 0) (String.concatWith "|" (List.map (toStringPrec 0) rs))
       | Not r => paren (d > 4) ("!" ^ toStringPrec 0 r)
in
   val toString = toStringPrec 0
end

fun nullable Any = false
  | nullable None = false
  | nullable Epsilon = true
  | nullable (SymSet _) = false
  | nullable (Closure _) = true
  | nullable (Concat rs) = List.all nullable rs
  | nullable (Or rs) = List.exists nullable rs
  | nullable (And rs) = List.all nullable rs
  | nullable (Not r) = Bool.not (nullable r)

fun delta r = if nullable r then Epsilon else None

(* fun derivative a = *)
(*    fn Any => Epsilon *)
(*     | None => None *)
(*     | Epsilon => None *)
(*     | SymSet s => if SymSet.member (s, a) then Epsilon else None *)
(*     | Closure r => concat (derivative a r, Closure r) *)
(*     | Concat [] => None *)
(*     | Concat [r] => derivative a r *)
(*     | Concat (r::rs) => or (concat (derivative a r, Concat rs), concat (delta r, derivative a (Concat rs))) *)
(*     | Or rs => Or (map (derivative a) rs) *)
(*     | And rs => And (map (derivative a) rs) *)
(*     | Not r => Not (derivative a r) *)

(* fun derivString [] r = r *)
(*   | derivString (c :: cs) r = derivString cs (derivative c r) *)

fun quotient (re1 as SymSet s1, re2) =
   (case re2 of
      Epsilon => None
    | Any => Epsilon
    | None => None
    | SymSet s2 => if SymSet.isEmpty (SymSet.intersect (s1, s2)) then none else epsilon
    | Closure r => concat (quotient (re1, r), re2)
    | Concat [] => none
    | Concat (r :: rs) => or (concat (quotient (re1, r), Concat rs),
                              concat (delta r, quotient (re1, Concat rs)))
    | Or rs => orList (List.map (fn r => quotient (re1, r)) rs)
    | And rs => andList (List.map (fn r => quotient (re1, r)) rs)
    | Not r => not (quotient (re1, r))
    (* end case *))
  | quotient (Epsilon, re2) = re2
  | quotient (None, _) = None
  | quotient (Any, re2) = quotient (SymSet SymSet.universe, re2)
  | quotient (Concat [], re2) = re2
  | quotient (Concat (r :: rs), re2) = quotient (Concat rs, quotient (r, re2))
  | quotient (Or rs, re2) = orList (List.map (fn r => quotient (r, re2)) rs)
  | quotient (And rs, re2) = andList (List.map (fn r => quotient (r, re2)) rs)
  | quotient (Closure r, re2) = or (re2, concat (quotient (r, re2), Closure r))
  | quotient (Not r, re2) = not (quotient (r, re2))

(* fun derivReg Epsilon r = r *)
(*   | derivReg None _ = None *)
(*   | derivReg Any r = *) 

end

(* vim: set tw=0 ts=3 sw=3: *)
