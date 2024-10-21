
structure Void:
   sig
      type t

      val absurd: t -> 'a
   end =
   struct
      type t = unit

      fun absurd () = raise Fail "Void.absurd"
   end

(* This can be "remembered" via casts *)
structure Top:
   sig
      type t

      val forget: 'a -> t
   end =
   struct
      type t = exn

      fun forget x = Unsafe.cast x
   end

structure Coercion =
struct

datatype ('a, 'b) t = T
type ('a, 'b) cast = ('a, 'b) t

val refl: ('a, 'a) t = T
fun sym (T: ('a, 'b) t): ('b, 'a) t = T
fun transport (T: ('a, 'b) t): 'a -> 'b = Unsafe.cast

(* In SML/NJ, ints and words are runtime equal *)
val intToWord: (int, word) t = T

(* Lifting coercions over datatypes *)
fun array (T: ('a, 'b) t): ('a array, 'b array) t = T
fun arrow (T: ('a, 'c) t, T: ('b, 'd) t): ('a -> 'b, 'c -> 'd) t = T
fun vector (T: ('a, 'b) t): ('a vector, 'b vector) t = T
fun ref (T: ('a, 'b) t): ('a ref, 'b ref) t = T
fun option (T: ('a, 'b) t): ('a option, 'b option) t = T
fun list (T: ('a, 'b) t): ('a list, 'b list) t = T

fun unList (T: ('a list, 'b list) t): ('a, 'b) t = T

functor Universal1(type 'a t):
   sig
      type hidden
      type f

      val to: hidden t -> f
      val from: f -> 'a t
   end =
   struct
      type hidden = exn
      type f = hidden t
      val to = Unsafe.cast
      val from = Unsafe.cast
   end

functor Universal2(type ('a, 'b) t):
   sig
      type hidden
      type 'a f

      val to: ('a, hidden) t -> 'a f
      val from: 'a f -> ('a, 'b) t
   end =
   struct
      type hidden = exn
      type 'a f = ('a, hidden) t
      val to = Unsafe.cast
      val from = Unsafe.cast
   end

datatype 'a list_app = ListApp of
     {pure: 'a -> 'a list,
      both: 'a list -> 'a list -> ('a * 'a) list}

structure LA = Universal1(type 'a t = 'a list_app)

val std' = ListApp {pure = fn x => [x], both = fn xs => List.concat o List.map (fn y => List.map (fn x => (x, y)) xs) }
val std = LA.to std'

val zip' = ListApp {pure = fn x => List.tabulate (100, fn _ => x),
                    both = fn xs => fn ys => ListPair.map (fn (x, y) => (x, y)) (xs, ys)}
val zip = LA.to zip'

fun test la =
   let
      val ListApp {pure, both} = LA.from la
   in
      both (pure 1) (pure 2)
   end

functor Exists(type 'a t):
   sig
      type 'a f = 'a t
      type t

      val to: 'a f -> t
      val from: (Void.t f -> 'b) -> t -> 'b
   end =
   struct
      type 'a f = 'a t
      type t = exn t

      val to = Unsafe.cast
      fun from k x = k (Unsafe.cast x)
   end

functor Exists2(type ('a, 'b) t):
   sig
      type ('a, 'b) f = ('a, 'b) t
      type 'a t
      type top

      val to: ('a, 'b) f -> 'a t
      val from: (('a, top) f -> 'b) -> 'a t -> 'b
   end =
   struct
      type ('a, 'b) f = ('a, 'b) t
      type 'a t = ('a, exn) t
      type top = exn

      val to = Unsafe.cast
      fun from k x = k (Unsafe.cast x)
   end

(*
datatype 'a foo =
   D1 : int -> string foo
 | D2 : bool foo
 | D3 : 'a list * 'a list -> 'a foo
*)

datatype ('a, 'b) foo_impl =
   D1' of int * ('a, string) t
 | D2' of ('a, bool) t
 | D3' of ('b * 'b) * ('a list, 'b) t

structure Foo = Exists2(type ('a, 'b) t = ('a, 'b) foo_impl)

type 'a foo = 'a Foo.t
val D1: int -> string foo           = fn n => Foo.to (D1' (n, refl))
val D2: bool foo                    = Foo.to (D2' refl)
val D3: 'a list * 'a list -> 'a foo = fn x => Foo.to (D3' (x, refl))

(*
fun bar (D1 n) = Int.toString n
  | bar D2 = true
  | bar (D3 (xs, ys)) = List.hd (xs @ ys)
*)

local
   fun k (D1' (n, c)) = transport (sym c) (Int.toString n)
     | k (D2' c) = transport (sym c) true
     | k (D3' ((xs, ys), c)) = let val zs = transport (sym c) xs @ transport (sym c) ys in hd zs end
   val _: ('a, Foo.top) foo_impl -> 'a = k
in
   fun bar d = Foo.from k d
end


(*
datatype 'a term =
   Lit : int -> int term
 | Pair : 'a term * 'b term -> ('a * 'b) term
*)

local
   datatype top1 = Top1
   datatype top2 = Top2
   datatype ('a, 'b, 'c) term =
      Lit' of int * ('a, int) cast
    | Pair' of ('b, top1, top2) term * ('c, top1, top2) term * ('b * 'c, 'a) cast

   fun Lit n = Lit' (n, refl)
   fun Pair (a: ('a, top1, top2) term, b: ('b, top1, top2) term): ('a * 'b, top1, top2) term = Unsafe.cast (Pair' (a, b, refl))
in
   type 'a term = ('a, top1, top2) term
   val Lit: int -> int term = Lit
   val Pair: 'a term * 'b term -> ('a * 'b) term = Pair
end
   

(* fun Foo (x: ('a, 'b) foo_impl): 'a foo = Foo' (Unsafe.cast x) *)
(* fun unFoo (Foo' x: 'a foo): ('a, 'b) foo_impl = Unsafe.cast x *)

(* fun D1 n = Foo' (D1' (n, refl)) *)
(* val D2 = Foo' (D2' refl) *)
(* fun D3 (xs: 'a list, ys: 'a list): 'a foo = *)
(*    Foo' (D3' ((Existential.to xs, Existential.to ys), sym Existential.unsafeFrom)) *)


(* datatype 'a foo = *)
(*    D1 of int * ('a, string) t *)
(*  | D2 of ('a, bool) t *)

(* fun d1 n = D1 (n, refl) *)
(* val d2 = D2 refl *)

(* fun bar (D1 (n, c): 'a foo): 'a = transport (sym c) (Int.toString n) *)
(*   | bar (D2 c) = transport (sym c) true *)

end

(* vim: set tw=0 ts=3 sw=3: *)
