
functor FingerTree(
T:
sig
   type item
   type annot
   val measure: item -> annot
   val annot_zero: annot
   val annot_plus: annot * annot -> annot
end):
sig
   type item = T.item
   type annot = T.annot
   type t

   val measure: t -> annot
   val empty: t
   val singleton: item -> t
   val cons: item * t -> t
   val snoc: t * item -> t
   val foldl: (item * 'a -> 'a) -> 'a -> t -> 'a
   val foldr: (item * 'a -> 'a) -> 'a -> t -> 'a
   val toList: t -> item list
   val fromList: item list -> t

   val viewl: t -> (item * t) option
   val viewr: t -> (t * item) option
end =
struct
   type item = T.item

   type annot = T.annot

   structure Elem:
   sig
      datatype t =
         Elem of item
       | Node2 of annot * t * t
       | Node3 of annot * t * t * t

      val measure: t -> annot
      val foldl: (item * 'a -> 'a) -> 'a -> t -> 'a
      val foldr: (item * 'a -> 'a) -> 'a -> t -> 'a
      val node2: t * t -> t
      val node3: t * t * t -> t
   end =
   struct
      datatype t =
         Elem of item
       | Node2 of annot * t * t
       | Node3 of annot * t * t * t

      fun measure (Elem x) = T.measure x
        | measure (Node2 (m, _, _)) = m
        | measure (Node3 (m, _, _, _)) = m

      fun foldl f z (Elem a) = f (a, z)
        | foldl f z (Node2 (_, a, b)) = foldl f (foldl f z a) b
        | foldl f z (Node3 (_, a, b, c)) = foldl f (foldl f (foldl f z a) b) c

      fun foldr f z (Elem a) = f (a, z)
        | foldr f z (Node2 (_, a, b)) = foldr f (foldr f z b) a
        | foldr f z (Node3 (_, a, b, c)) = foldr f (foldr f (foldr f z c) b) a

      local
         val op + = T.annot_plus
      in
         fun node2 (a, b) =
            Node2 (measure a + measure b, a, b)
         fun node3 (a, b, c) =
            Node3 (measure a + measure b + measure c, a, b, c)
      end
   end

   structure Digit:
   sig
      datatype t =
         One of Elem.t
       | Two of Elem.t * Elem.t
       | Three of Elem.t * Elem.t * Elem.t
       | Four of Elem.t * Elem.t * Elem.t * Elem.t

      val foldl: (item * 'a -> 'a) -> 'a -> t -> 'a
      val foldr: (item * 'a -> 'a) -> 'a -> t -> 'a
      val measure: t -> annot
   end =
   struct
      datatype t =
         One of Elem.t
       | Two of Elem.t * Elem.t
       | Three of Elem.t * Elem.t * Elem.t
       | Four of Elem.t * Elem.t * Elem.t * Elem.t

      fun foldl f z (One a) = Elem.foldl f z a
        | foldl f z (Two (a, b)) = Elem.foldl f (Elem.foldl f z a) b
        | foldl f z (Three (a, b, c)) = Elem.foldl f (Elem.foldl f (Elem.foldl f z a) b) c
        | foldl f z (Four (a, b, c, d)) = Elem.foldl f (Elem.foldl f (Elem.foldl f (Elem.foldl f z a) b) c) d

      fun foldr f z (One a) = Elem.foldr f z a
        | foldr f z (Two (a, b)) = Elem.foldr f (Elem.foldr f z b) a
        | foldr f z (Three (a, b, c)) = Elem.foldr f (Elem.foldr f (Elem.foldr f z c) b) a
        | foldr f z (Four (a, b, c, d)) = Elem.foldr f (Elem.foldr f (Elem.foldr f (Elem.foldr f z d) c) b) a

      local
         val op + = T.annot_plus
         val m = Elem.measure
      in
         fun measure (One x) = m x
           | measure (Two (x, y)) = m x + m y
           | measure (Three (x, y, z)) = m x + m y + m z
           | measure (Four (x, y, z, w)) = m x + m y + m z + m w
      end
   end

   datatype t =
      Empty
    | Single of Elem.t
    | Deep of annot * Digit.t * t * Digit.t (* TODO: Susp.susp *)

   fun measure Empty = T.annot_zero
     | measure (Single x) = Elem.measure x
     | measure (Deep (m, _, _, _)) = m

   fun deep (a, b, c) =
      let
         val op + = T.annot_plus
         val m = Digit.measure a + measure b + Digit.measure c
      in
         Deep (m, a, b, c)
      end

   val empty = Empty
   
   fun singleton x = Single (Elem.Elem x)

   fun cons (x, xs) =
      let
         fun go (a, Empty) = Single a
           | go (a, Single b) = deep (Digit.One a, Empty, Digit.One b)
           | go (a, Deep (_, pr, m, sf)) =
            case pr of
               Digit.One b => deep (Digit.Two (a, b), m, sf)
             | Digit.Two (b, c) => deep (Digit.Three (a, b, c), m, sf)
             | Digit.Three (b, c, d) => deep (Digit.Four (a, b, c, d), m, sf)
             | Digit.Four (b, c, d, e) => deep (Digit.Two (a, b), go (Elem.node3 (c, d, e), m), sf)
      in
         go (Elem.Elem x, xs)
      end

   fun snoc (xs, x) =
      let
         fun go (a, Empty) = Single a
           | go (a, Single b) = deep (Digit.One b, Empty, Digit.One a)
           | go (a, Deep (_, pr, m, sf)) =
            case sf of
               Digit.One b => deep (pr, m, Digit.Two (b, a))
             | Digit.Two (b, c) => deep (pr, m, Digit.Three (b, c, a))
             | Digit.Three (b, c, d) => deep (pr, m, Digit.Four (b, c, d, a))
             | Digit.Four (b, c, d, e) => deep (pr, go (Elem.node3 (b, c, d), m), Digit.Two (e, a))
      in
         go (Elem.Elem x, xs)
      end

   fun foldr _ z Empty = z
     | foldr f z (Single x) = Elem.foldr f z x
     | foldr f z (Deep (_, pr, m, sf)) = Digit.foldr f (foldr f (Digit.foldr f z sf) m) pr

   fun foldl _ z Empty = z
     | foldl f z (Single x) = Elem.foldl f z x
     | foldl f z (Deep (_, pr, m, sf)) = Digit.foldl f (foldl f (Digit.foldl f z pr) m) sf

   fun toList x = foldr op:: [] x

   fun fromList xs = List.foldr cons empty xs

   fun viewl x =
      let
         fun go Empty = NONE
           | go (Single x) = SOME (x, Empty)
           | go (Deep (_, pr, m, sf)) = raise Fail "NYI"
         fun unElem (Elem.Elem x) = x
           | unElem _ = raise Fail "Finger tree invariant broken"
      in
         Option.map (fn (a, b) => (unElem a, b)) (go x)
      end

   fun viewr _ = NONE
     (* | viewr (Single x) = SOME (x, Empty) *)
     (* | viewr (Deep (_, pr, m, sf)) = NONE *)
end

structure Seq = FingerTree(
   type annot = int
   type item = string
   fun measure _ = 1
   val annot_zero = 0
   val annot_plus = Int.+)

(* vim: set tw=0 ts=3 sw=3: *)
