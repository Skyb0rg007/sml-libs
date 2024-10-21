
structure CFA =
struct

structure Var =
   struct
      datatype t = T of string * int

      val counter = ref 0

      fun new str = T (str, !counter) before counter := !counter + 1

      fun toString (T (s, n)) = s ^ "_" ^ Int.toString n

      fun equals (T (_, n), T (_, m)) = n = m

      structure Map = RedBlackMapFn(
         struct
            type ord_key = t

            fun compare (T (_, n), T (_, m)) = Int.compare (n, m)
         end)
   end

structure CPS =
   struct
      datatype node =
         LETFUN of lambda list * t
       | LETLIT of Var.t * int * t
       | APPLY of Var.t * Var.t list

      and t = T of node * int

      withtype lambda = Var.t * Var.t list * t

      val counter = ref 0

      fun make k args = T (k args, !counter) before counter := !counter + 1

      val letfun = make LETFUN
      val letlit = make LETLIT
      val apply = make APPLY
   end

structure Eval =
   struct
      type time = int

      local
         val counter = ref 0
      in
         fun alloc () = !counter before counter := !counter + 1
      end

      type benv = time Var.Map.map

      datatype value =
         Int of int
       | Clo of benv * CPS.lambda

      type addr = value * time

      type store = (addr, value) HashTable.hash_table

      fun step (exp, benv, store) =
         case exp of
            CPS.LETLIT (v, n, k) =>
               let
                  val t = alloc ()
                  val addr = (v, t)
                  val benv' = Var.Map.insert (benv, v, t)
               in
                  HashTable.insert store (addr, Int n)
                  ; (k, benv')
               end
          | CPS.LETFUN ([lam as (f, _, _)], k) =>
               let
                  val t = alloc ()
                  val addr = (f, t)
                  val benv' = Var.Map.insert (benv, f, t)
               in
                  HashTable.insert store (addr, Clo (benv, lam))
                  ; (k, benv')
               end
          | CPS.LETFUN _ => raise Fail "Mutual recursion NYI"
          | CPS.APPLY (f, args) =>
               let
                  val (benv', (_, params, body)) =
                     case HashTable.lookup store (f, Var.Map.lookup (benv, f)) of
                        Int _ => raise Fail "Application of integer"
                      | Clo c => c
               in
                  ListPair.appEq
                     (fn (param, arg) => HashTable.insert store (()))
                  (body, benv')
               end
   end

val id = Var.new "id"
val x = Var.new "x"
val k = Var.new "k"
val halt = Var.new "halt"

val e =
   CPS.letfun ([(id, [x, k], CPS.apply (k, [x]))],
   CPS.apply (halt, [id]))

end

(* vim: set tw=0 ts=3 sw=3: *)
