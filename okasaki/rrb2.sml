
signature RRB2 =
   sig
      type 'a vector

      val empty : 'a vector
      val length : 'a vector -> int
   end

structure RRB2 : RRB2 =
   struct
      structure A = Array
      structure V = Vector

      datatype 'a tree = Balanced of 'a tree A.array
                       | Unbalanced of 'a tree A.array * int V.vector
                       | Leaf of 'a A.array

      datatype 'a vector = Empty | Root of int * word * 'a tree

      val empty = Empty

      fun length Empty = 0
        | length (Root (n, _, _)) = n
   end

(* vim: set ts=3 sw=3: *)
