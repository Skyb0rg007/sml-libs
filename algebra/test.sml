
structure Tree =
   struct
      datatype 'a t = Bin of 'a t * 'a * 'a t
                    | Tip of 'a

      fun map f (Bin (l, x, r)) = Bin (map f l, f x, map f r)
        | map f (Tip x) = Tip (f x)

      fun root (Bin (_, x, _)) = x
        | root (Tip x) = x

      datatype 'a ctx = BinL of 'a ctx * 'a * 'a t
                      | BinR of 'a t * 'a * 'a ctx
                      | Root

      datatype 'a zipper = Zipper of 'a ctx * 'a t

      fun toZipper t = Zipper (Root, t)

      fun up (Zipper (Root, t)) = Zipper (Root, t)
        | up (Zipper (BinL (l, x, r), t)) = Zipper (l, Bin (t, x, r))
        | up (Zipper (BinR (l, x, r), t)) = Zipper (r, Bin (l, x, t))

      fun left (Zipper (c, Tip x)) = Zipper (c, Tip x)
        | left (Zipper (c, Bin (l, x, r))) = Zipper (BinL (c, x, r), l)

      fun right (Zipper (c, Tip x)) = Zipper (c, Tip x)
        | right (Zipper (c, Bin (l, x, r))) = Zipper (BinR (l, x, c), r)

      fun current (Zipper (_, t)) = t

      fun fromZipper (Zipper (Root, t)) = t
        | fromZipper z = fromZipper (up z)
   end

structure Test =
   struct

      structure Q = IntSubQuasigroup

      datatype 'a tree = Bin of 'a tree * 'a * 'a tree
                       | Tip of 'a

      fun root (Bin (_, x, _)) = x
        | root (Tip x) = x

      fun map f (Tip x) = Tip (f x)
        | map f (Bin (l, x, r)) = Bin (map f l, f x, map f r)

      val _ : ('a -> 'b) -> 'a tree -> 'b tree = map
      
      fun annotate f acc (Tip x) = Tip (f (x, acc), x)
        | annotate f acc (Bin (l, x, r)) =
         let
            val l' = annotate f acc l
            val acc = f (x, #1 (root l'))
            val r' = annotate f acc r
         in
            Bin (l', (acc, x), r')
         end

      val _ : ('a * 'b -> 'b) -> 'b -> 'a tree -> ('b * 'a) tree = annotate


   end
  
(* vim: set ts=3 sw=3 :*)
