structure IntList =
struct
  structure F =
    struct
      datatype 'a t = Nil | Cons of int * 'a

      fun map _ Nil = Nil
        | map f (Cons (a, b)) = Cons (a, f b)

    end

  type t = int list

  fun project [] = F.Nil
    | project (x :: xs) = F.Cons (x, xs)

  fun embed F.Nil = []
    | embed (F.Cons (x, xs)) = x :: xs
end
structure IntList =
struct
  structure S = SchemesAll(IntList)
  open S
  open IntList
end

structure Tree =
struct
  structure F =
    struct
      datatype 'a t =
         Empty
       | Node of 'a * int * 'a

      fun map f (Node (l, m, r)) = Node (f l, m, f r)
        | map _ Empty = Empty
    end
  datatype t =
     Empty
   | Node of t * int * t

  fun embed (F.Node (l, m, r)) = Node (l, m, r)
    | embed F.Empty = Empty
  fun project (Node (l, m, r)) = F.Node (l, m, r)
    | project Empty = F.Empty
end
structure Tree =
struct
  structure S = SchemesAll(Tree)
  open S
  open Tree
end

structure Prog =
struct
  structure F =
    struct
      datatype 'a t =
         Ret of int
       | Put of int * int * 'a
       | Get of int * (int -> 'a)

      fun map _ (Ret x) = Ret x
        | map f (Put (a, b, c)) = Put (a, b, f c)
        | map f (Get (a, b)) = Get (a, f o b)
    end

  datatype t =
     Ret of int
   | Put of int * int * t
   | Get of int * (int -> t)

  fun project (Ret x) = F.Ret x
    | project (Put (a, b, c)) = F.Put (a, b, c)
    | project (Get (a, b)) = F.Get (a, b)

  fun embed (F.Ret x) = Ret x
    | embed (F.Put (a, b, c)) = Put (a, b, c)
    | embed (F.Get (a, b)) = Get (a, b)
end
structure Prog =
struct
  structure S = SchemesAll(Prog)
  open S
  open Prog
end

structure Nat =
struct
  structure F =
    struct
      datatype 'a t = Zero | Succ of 'a

      fun map _ Zero = Zero
        | map f (Succ x) = Succ (f x)
    end

  datatype t = Zero | Succ of t

  fun plus (x, Zero) = x
    | plus (x, Succ y) = Succ (plus (x, y))

  fun mul (x, Zero) = Zero
    | mul (x, Succ y) = plus (x, mul (x, y))

  fun fromInt n = 
      if n <= 0
        then Zero
      else Succ (fromInt (n - 1))

  fun toInt Zero = 0
    | toInt (Succ n) = 1 + toInt n

  fun project Zero = F.Zero
    | project (Succ x) = F.Succ x

  fun embed F.Zero = Zero
    | embed (F.Succ x) = Succ x
end
structure Nat =
struct
  structure S = SchemesAll(Nat)
  open S
  open Nat
end

structure Test =
struct
  (* 3.1 Catamorphisms *)
  local
    fun alg _ z IntList.F.Nil = z
      | alg f _ (IntList.F.Cons (a, x)) = f (a, x)
  in
    fun foldr' f z = IntList.cata (alg f z)
    val _: (int * 'a -> 'a) -> 'a -> int list -> 'a = foldr'
  end

  local
    fun alg Tree.F.Empty = 0
      | alg (Tree.F.Node (l, _, r)) = 1 + l + r
  in
    val size = Tree.cata alg
    val _: Tree.t -> int = size
  end

  local
    fun alg (Prog.F.Ret x) _ = x
      | alg (Prog.F.Put (i, x, k)) m = k (IntBinaryMap.insert (m, i, x))
      | alg (Prog.F.Get (i, k)) m = k (IntBinaryMap.lookup (m, i)) m
  in
    fun interp x = Prog.cata alg x
    val _: Prog.t -> int IntBinaryMap.map -> int = interp
  end

  local
    fun alg' a Nat.F.Zero = a (Nat.Succ Nat.Zero)
      | alg' a (Nat.F.Succ b) = a b

    fun alg Nat.F.Zero = Nat.Succ
      | alg (Nat.F.Succ a) = Nat.cata (alg' a)
  in
    val ack = Fn.uncurry (Nat.cata alg)
    val _: Nat.t * Nat.t -> Nat.t = ack
  end

  (* 3.2 Anamorphisms *)
  local
    fun coalg g b =
      case g b of
          NONE => IntList.F.Nil
        | SOME (a, b) => IntList.F.Cons (a, b)
  in
    fun unfoldr' g = IntList.ana (coalg g)
    val _: ('a -> (int * 'a) option) -> 'a -> int list = unfoldr'
  end

  local
    fun coalg ([], []) = IntList.F.Nil
      | coalg (x::xs, []) = IntList.F.Cons (x, (xs, []))
      | coalg ([], y::ys) = IntList.F.Cons (y, ([], ys))
      | coalg (x::xs, y::ys) =
        if x < y
          then IntList.F.Cons (x, (xs, y::ys))
        else IntList.F.Cons (y, (x::xs, ys))
  in
    fun merge (x, y) = IntList.ana coalg (x, y)
    val _: int list * int list -> int list = merge
  end

  (* 3.3 Hylomorphisms *)
  local
    fun partition [] = Tree.F.Empty
      | partition (x::xs) =
        case List.partition (fn y => y < x) xs of
          (l, r) => Tree.F.Node (l, x, r)

    fun combine Tree.F.Empty = []
      | combine (Tree.F.Node (l, m, r)) = l @ [m] @ r
  in
    fun qsort xs = Tree.hylo combine partition xs
    val _: int list -> int list = qsort
  end

  local
    fun sum IntList.F.Nil = 0
      | sum (IntList.F.Cons (a, b)) = a + b

    fun iota 0 = IntList.F.Nil
      | iota n = IntList.F.Cons (n, n - 1)
  in
    fun sumUpTo n = IntList.hylo sum iota n
    val _: int -> int = sumUpTo
  end

  (* 5.1 Mutumorphism *)
  local
    fun alg1 Nat.F.Zero = 0
      | alg1 (Nat.F.Succ (n, m)) = n + m

    fun alg2 Nat.F.Zero = 1
      | alg2 (Nat.F.Succ (n, _)) = n
  in
    fun fib x = #1 (Nat.mutu alg1 alg2) (Nat.fromInt x)
    val _: int -> int = fib
  end

  (* 6.1 Paramorphisms *)
  local
    fun alg Nat.F.Zero = Nat.Succ Nat.Zero
      | alg (Nat.F.Succ (n, f)) = Nat.mul (Nat.Succ n, f)
  in
    fun fact x = Nat.para alg x
  end

  (* 6.2 Apomorphisms *)
  local
    fun coalg f [] = IntList.F.Nil
      | coalg f (x::xs) = IntList.F.Cons (f x, Either.INL xs)
  in
    fun maphd f = IntList.apo (coalg f)
  end

  local
    fun coalg y [] = IntList.F.Cons (y, Either.INL [])
      | coalg y (x::xs) =
        if y <= x
          then IntList.F.Cons (y, Either.INL (x::xs))
        else IntList.F.Cons (x, Either.INR xs)
  in
    fun insert y = IntList.apo (coalg y)
  end

  (* 6.3 Zygomorphisms *)
  local
    fun perf Tree.F.Empty = true
      | perf (Tree.F.Node ((l, dl: int), _, (r, dr))) = l andalso r andalso dl = dr

    fun depth Tree.F.Empty = 0
      | depth (Tree.F.Node (d1, _, d2)) = 1 + (Int.max (d1, d2))
  in
    fun perfect t = Tree.zygo perf depth t
  end

  (* 7.2 Dynamorphisms *)
  local
    structure F =
    struct
      datatype 'a t = T of int list * int list * 'a option

      fun map f (T (xs, ys, x)) = T (xs, ys, Option.map f x)
    end
    structure Cofree = FreeComonadFn(F)
    structure Dyna = Dynamorphism(
      struct
        structure F = F
        structure Cofree = Cofree
      end)

    fun index 0 t = Cofree.extract t
      | index n (Cofree.:< (_, F.T (_, _, SOME t))) = index (n - 1) t
      | index _ _ = raise Subscript

    fun coalg _  ([], []) = F.T ([], [], NONE)
      | coalg zs ([], y::ys) = F.T ([], y::ys, SOME (zs, ys))
      | coalg _  (x::xs, ys) = F.T (x::xs, ys, SOME (xs, ys))

    fun alg n (F.T (_, _, NONE)) = []
      | alg n (F.T ([], _, SOME _)) = []
      | alg n (F.T (_, [], SOME _)) = []
      | alg n (F.T (x::_, y::_, SOME h)) =
        if x = y
          then x :: index (n + 1) h
        else
          case (Cofree.extract h, index n h) of
            (x1, x2) =>
              if List.length x1 > List.length x2 then x1 else x2
  in
    fun lcs (xs, ys) = Dyna.dyna (alg (List.length xs)) (coalg xs) (xs, ys)
  end

  (* structure Cofree = FreeComonadFn(TreeF) *)
  (* structure Cata = Catamorphism( *)
  (*   struct *)
  (*     structure F = TreeF *)
  (*     type t = Tree.t *)
  (*     val project = Tree.project *)
  (*   end) *)
  (* structure Ana = Anamorphism( *)
  (*   struct *)
  (*     structure F = TreeF *)
  (*     type t = Tree.t *)
  (*     val embed = Tree.embed *)
  (*   end) *)
  (* structure Histo = Histomorphism( *)
  (*   struct *)
  (*     structure F = TreeF *)
  (*     structure Cofree = Cofree *)
  (*     type t = Tree.t *)
  (*     val project = Tree.project *)
  (*   end) *)
  (* structure Hylo = Hylomorphism( *)
  (*   struct *)
  (*     structure F = TreeF *)
  (*   end) *)

  (* infix 5 :< *)
  (* datatype cofree = datatype Cofree.t *)

  (* fun rollup [_ :< TreeF.NodeF (node, cofrees)] = *)
  (*       let *)
  (*         val (nodes, label) = rollup cofrees *)
  (*       in *)
  (*         (node :: nodes, label) *)
  (*       end *)
  (*   | rollup cofrees = ([], List.map Cofree.extract cofrees) *)

  (* fun pprint t = *)
  (*   let *)
  (*     fun alg (TreeF.NodeF (node, cofrees)) indent = *)
  (*       let *)
  (*         val (nodes, fs) = rollup cofrees *)
  (*         val ss = List.map (fn f => f (indent + 2)) fs *)
  (*         val s = CharVector.tabulate (indent, fn _ => #" ") *)
  (*               ^ "* " *)
  (*               ^ String.concatWith " / " (node :: nodes) *)
  (*       in *)
  (*         String.concatWith "\n" (s :: ss) *)
  (*       end *)
  (*   in *)
  (*     Histo.histo alg t 0 *)
  (*   end *)

  (* structure ListF = *)
  (*   struct *)
  (*     datatype 'a t = NilF | ConsF of IntInf.int * 'a *)

  (*     fun map _ NilF = NilF *)
  (*       | map f (ConsF (x, y)) = ConsF (x, f y) *)
  (*   end *)

  (* structure ListSchemes = SchemesAll( *)
  (*   struct *)
  (*     structure F = ListF *)
  (*     type t = IntInf.int list *)

  (*     fun embed ListF.NilF = [] *)
  (*       | embed (ListF.ConsF (x, xs)) = x :: xs *)

  (*     fun project [] = ListF.NilF *)
  (*       | project (x :: xs) = ListF.ConsF (x, xs) *)
  (*   end) *)

  (* val collatzLength = *)
  (*   let *)
  (*     fun alg ListF.NilF = raise Fail "Nil!" *)
  (*       | alg (ListF.ConsF (_, x)) = x + 1 *)

  (*     fun coalg 1 = Either.INL 1 *)
  (*       | coalg n = *)
  (*           if n mod 2 = 0 *)
  (*             then Either.INR (ListF.ConsF (n, n div 2)) *)
  (*           else Either.INR (ListF.ConsF (n, 3 * n + 1)) *)
  (*   in *)
  (*     ListSchemes.elgot alg coalg *)
  (*   end *)

  (* fun run () = *)
  (*   let *)
  (*     fun sizeAlg (TreeF.NodeF (_, xs)) = 1 + List.foldr op + 0 xs *)
  (*     val size = Cata.cata sizeAlg *)

  (*     fun iota n = *)
  (*       let *)
  (*         fun go (n, acc) = *)
  (*               if n <= 0 *)
  (*                 then acc *)
  (*               else go (n - 1, n :: acc) *)
  (*       in *)
  (*         go (n, []) *)
  (*       end *)

  (*     fun coalg 0 = TreeF.NodeF ("0", []) *)
  (*       | coalg n = TreeF.NodeF (Int.toString n, [n - 1]) *)

  (*     val t = Ana.ana coalg 10 *)
  (*     (1* val () = TextIO.print (pprint t ^ "\n") *1) *)

  (*     fun coalg 0 = TreeF.NodeF ("0", []) *)
  (*       | coalg n = TreeF.NodeF (Int.toString n, iota (n - 1)) *)
  (*     val t = Ana.ana coalg 10 *)
      (* val () = TextIO.print ("size: " ^ Int.toString (size t) ^ "\n") *)

      (* val () = TextIO.print ("size: " ^ Int.toString (Hylo.hylo sizeAlg coalg 10) ^ "\n") *)
      (* val t = Ana.ana coalg 100 *)
      (* val () = TextIO.print("Done constructing size 100\n") *)
      (* val () = TextIO.print ("size: " ^ Int.toString (Hylo.hylo sizeAlg coalg 22) ^ "\n") *)
      (* val () = TextIO.print ("size: " ^ Int.toString (Cata.cata sizeAlg (Ana.ana coalg 22)) ^ "\n") *)

    (* in *)
    (*   () *)
    (* end *)
end
