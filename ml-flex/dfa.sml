
structure DFA =
struct

datatype t = DFA of {
      last: int ref,
      (* State to enter upon reading a character *)
      next: int vector,
      (* Check value to see if 'next' applies *)
      check: int vector,
      (* Offset into 'next' for a given state *)
      base: int vector,
      (* Where to go if 'check' disallows 'next' entry *)
      def: int vector,
      (* Accepting number for each state *)
      acc: int vector
   }

end

(* vim: set tw=0 ts=3 sw=3: *)
