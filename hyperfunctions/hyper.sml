
structure Hyper:
sig
   datatype ('a, 'b) t = Hyper of ('b, 'a) t -> 'b

   val invoke: ('a, 'b) t -> ('b, 'a) t -> 'b
   val unroll: ('a, 'b) t -> (('a, 'b) t -> 'a) -> 'b
   val roll: ((('a, 'b) t -> 'a) -> 'b) -> ('a, 'b) t
   val ana: ('x * ('x -> 'a) -> 'b) -> 'x -> ('a, 'b) t
   val cata: ((('x -> 'a) -> 'b) -> 'x) -> ('a, 'b) t -> 'x
   val run: ('a, 'a) t -> 'a
   val push: ('a -> 'b) -> ('a, 'b) t -> ('a, 'b) t

   (* Category *)
   val id: ('a, 'a) t
   val compose: ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t

   (* Arrow *)
   val arr: ('a -> 'b) -> ('a, 'b) t

   (* Profunctor *)
   val lmap: ('c -> 'a) -> ('a, 'b) t -> ('c, 'b) t
   val rmap: ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
   val dimap: ('c -> 'a) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t

   (* Strong *)
   val first: ('a, 'b) t -> ('a * 'c, 'b * 'c) t
   val second: ('a, 'b) t -> ('c * 'a, 'c * 'b) t
   val *** : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
   val &&& : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t

   (* Costrong *)
   val unfirst: ((unit -> 'a) * (unit -> 'c), 'b * 'c) t -> ('a, 'b) t
   val feedback: 'c -> ('a * 'c, 'b * 'c) t -> ('a, 'b) t

   val zip: 'a list * 'b list -> ('a * 'b) list
end =
struct

datatype ('a, 'b) t = Hyper of ('b, 'a) t -> 'b

fun invoke (Hyper f) = f

fun unroll (Hyper f) = f o Hyper

fun roll f = Hyper (f o invoke)

fun ana psi =
   let
      fun go x = Hyper (fn z => psi (x, invoke z o go))
   in
      go
   end

fun cata phi =
   let
      fun go h = phi (fn g => unroll h (g o go))
   in
      go
   end

fun push f q = Hyper (fn k => f (invoke k q))

fun arr f =
   let
      fun go x = invoke (push f (Hyper go)) x
   in
      Hyper go
   end

val id = Hyper (fn Hyper k => k (arr (fn x => x)))

fun run f = invoke f id

fun compose f g = Hyper (fn k => invoke f (compose' g k))

and compose' g k = Hyper (fn f => invoke g (compose'' k f))

and compose'' k f = Hyper (fn g => invoke k (compose f g))

fun dimap f g h = Hyper (g o invoke h o dimap' g f)

and dimap' f g h = Hyper (g o invoke h o dimap g f)

fun lmap f h = Hyper (invoke h o rmap f)

and rmap f h = Hyper (f o invoke h o lmap f)

fun fst (a, _) = a
fun snd (_, b) = b

fun first h = ana (fn (i, fac) => (unroll i (fst o fac), snd (fac i))) h
fun second h = ana (fn (i, fca) => (fst (fca i), unroll i (snd o fca))) h

fun op *** hs =
   ana
   (fn ((i, j), fgac) => (unroll i (fn i' => fst (fgac (i', j))),
                          unroll j (fn j' => snd (fgac (i, j')))))
   hs

fun op &&& hs =
   ana
   (fn ((i, j), fga) => (unroll i (fn i' => fga (i', j)),
                         unroll j (fn j' => fga (i, j'))))
   hs

fun unfirst h =
   let
      fun go fa x =
         unroll x (fn i =>
            (fn () => fa i, fn () => snd (go fa i)))
   in
      ana (fn (i, fac) => fst (go fac i)) h
   end
(* fun ana psi = *)
(*    let *)
(*       fun go x = Hyper (fn z => psi (x, invoke z o go)) *)
(*    in *)
(*       go *)
(*    end *)

(* fun unfirst h = Hyper (fn z => *)
(*    let *)
(*       (1* i := h *1) *)
(*       (1* fac := invoke z o unfirst *1) *)

(*       fun go x = unroll x (fn i => (invoke z (unfirst i), fn () => snd (go i))) *)
(*    in *)
(*       fst (go h) *)
(*    end) *)

fun feedback (c: 'c) (Hyper (k: ('b * 'c, 'a * 'c) t -> 'b * 'c)): ('a, 'b) t =
   Hyper (fn a: ('b, 'a) t =>
   let
      val (b, c) = k (first a)
   in
      raise Fail ""
   end : 'b)

val _: 'c -> ('a * 'c, 'b * 'c) t -> ('a, 'b) t = feedback

fun ('a, 'b) zip (xs, ys) =
   let
      val xz: ('a -> ('a * 'b) list, ('a * 'b) list) t =
         let
            fun f (x, xk) = Hyper (fn yk => invoke yk xk x)
            val b = Hyper (fn _ => [])
         in
            List.foldr f b xs
         end

      val yz: (('a * 'b) list, 'a -> ('a * 'b) list) t =
         let
            fun f (y, yk) = Hyper (fn xk => fn x => (x, y) :: invoke xk yk)
            val b = Hyper (fn _ => fn _ => [])
         in
            List.foldr f b ys
         end
   in
      invoke xz yz
   end

end

structure Seq =
   struct
      datatype 'a node = Nil | Cons of 'a * 'a t
      withtype 'a t = unit -> 'a node

      fun nil_ () = Nil
      fun cons x xs () = Cons (x, xs)

      fun take n xs () =
         if n <= 0
            then Nil
         else
            case xs () of
               Nil => Nil
             | Cons (y, ys) => Cons (y, take (n - 1) ys)

      fun toList xs =
         case xs () of
            Nil => []
          | Cons (y, ys) => y :: toList ys

      local
         fun nats n () = Cons (n, nats (n + 1))
      in
         val nats = nats 0
      end
   end

(* vim: set tw=0 ts=3 sw=3: *)
