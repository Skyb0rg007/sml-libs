
structure DFA =
struct

structure IntSet = IntRedBlackSet

datatype t = DFA of {
      states: IntSet.set,
      initial: int,
      final: IntSet.set,
      delta: int * Word8.word -> int
   }

fun accepts (DFA {initial, final, delta, ...}) str =
   let
      fun go (s, []) = IntSet.member (final, s)
        | go (s, c :: cs) = go (delta (s, c), cs)
   in
      go (initial, str)
   end

fun complement (DFA {states, initial, final, delta}) =
   DFA {
      states = states,
      initial = initial,
      final = IntSet.difference (states, final),
      delta = delta
   }

fun intersect (dfa1, dfa2) =
   let
      val DFA {states = s1, initial = i1, final = f1, delta = d1} = dfa1
      val DFA {states = s2, initial = i2, final = f2, delta = d2} = dfa2
   in
      ()
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
