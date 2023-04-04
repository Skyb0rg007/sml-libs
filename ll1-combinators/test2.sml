
structure Test2 =
struct

infix <|>

datatype kind = K of int
datatype token = T of int

datatype 'a syntax =
   Pure of 'a
 | Map of (exn -> 'a) * exn syntax
 | LiftA2 of (exn * exn -> 'a) * exn syntax * exn syntax
 | Empty
 | Or of 'a syntax * 'a syntax
 | Kind of (token -> 'a) * kind
 | Rec of int * (unit -> 'a syntax)

val pure = Pure
val empty = Empty
val op <|> = Or
fun kind k = Kind (fn x => x, k)
fun map f (Map (g, x)) = Map (f o g, x)
  | map _ Empty = Empty
  | map f x =
   let
      exception A of 'a

      fun go (Pure a) = Pure (A a)
        | go (Map (g, y)) = Map (A o g, y)
        | go (LiftA2 (g, y, z)) = LiftA2 (A o g, y, z)
        | go Empty = Empty
        | go (Or (a, b)) = Or (go a, go b)
        | go (Kind (g, y)) = Kind (A o g, y)
        | go (Rec (n, g)) = Rec (n, go o g)

      fun f' (A a) = f a
        | f' e = raise Fail ("map: Impossible, got " ^ exnName e)
   in
      Map (f', go x)
   end
fun liftA2 f (x, y) =
   let
      exception A of 'a
      exception B of 'b

      fun f' (A a, B b) = f (a, b)
        | f' _ = raise Fail "liftA2: Impossible"
   in
      LiftA2 (f', map A x, map B y)
   end
local
   val counter = ref 0
in
   fun fix f =
      let
         val id = !counter
         val () = counter := !counter + 1
         fun node () = Rec (id, fn () => f (node ()))
      in
         node ()
      end
end

val _: 'a -> 'a syntax = pure
val _: 'a syntax = empty
val _: 'a syntax * 'a syntax -> 'a syntax = op <|>
val _: kind -> token syntax = kind
val _: ('a -> 'b) -> 'a syntax -> 'b syntax = map
val _: ('a * 'b -> 'c) -> 'a syntax * 'b syntax -> 'c syntax = liftA2
val _: ('a syntax -> 'a syntax) -> 'a syntax = fix

val cache: exn option option array = Array.array (100, NONE)

fun nullable (Pure a) = SOME a
  | nullable (Map (f, a)) = Option.map f (nullable a)
  | nullable (LiftA2 (f, a, b)) =
   (case (nullable a, nullable b) of
       (SOME x, SOME y) => SOME (f (x, y))
     | _ => NONE)
  | nullable Empty = NONE
  | nullable (Or (a, b)) =
   (case nullable a of
       NONE => nullable b
     | SOME v => SOME v)
  | nullable (Kind _) = NONE
  | nullable (Rec (id, f)) =
   case Array.sub (cache, id) of
      NONE =>
         let
            val () = Array.update (cache, id, SOME NONE);
            val r = nullable (f ())
         in
            Array.update (cache, id, SOME r);
            r
         end
    | SOME r => r

(* val nullable = fn args => (print "nullable\n"; nullable args) *)

val cache: bool option array = Array.array (100, NONE)

fun productive (Pure _) = true
  | productive (Map (_, a)) = productive a
  | productive (LiftA2 (_, a, b)) = productive a andalso productive b
  | productive Empty = false
  | productive (Or (a, b)) = productive a orelse productive b
  | productive (Kind (_, _)) = true
  | productive (Rec (id, f)) =
   case Array.sub (cache, id) of
      SOME r => r
    | NONE => 
         let
            val () = Array.update (cache, id, SOME false);
            val r = productive (f ())
         in
            Array.update (cache, id, SOME r);
            r
         end

(* val productive = fn args => (print "productive\n"; productive args) *)

val cache: bool option array option array = Array.array (100, NONE)

fun first (_, Pure _) = false
  | first (k, Map (_, s)) = first (k, s)
  | first (k, LiftA2 (_, s1, s2)) =
   let
      val () = print "first-before\n"
      val b =
   (first (k, s1) andalso productive s2)
   orelse
   (Option.isSome (nullable s1) andalso first (k, s2))
   in
      print "first-after\n";
      b
   end
  | first (_, Empty) = false
  | first (k, Or (s1, s2)) = first (k, s1) orelse first (k, s2)
  | first (K k, Kind (_, K k')) = k = k'
  | first (K k, Rec (id, f)) =
   case Array.sub (cache, id) of
      NONE =>
         let
            val r = first (K k, f ())
            val arr = Array.array (100, NONE)
         in
            Array.update (arr, k, SOME r);
            Array.update (cache, id, SOME arr);
            r
         end
    | SOME ks =>
         case Array.sub (ks, k) of
            NONE =>
               let
                  val r = first (K k, f ())
               in
                  Array.update (ks, k, SOME r);
                  r
               end
          | SOME b => b

(* val first = fn args => (print "first\n"; first args) *)

datatype layer =
   LMap of exn -> exn
 | LLiftA2 of (exn * exn -> exn) * exn syntax

datatype focus = Focus of exn syntax * layer list

fun plug (v, []) = Focus (pure v, [])
  | plug (v, LMap f :: c) = plug (f v, c)
  | plug (v, LLiftA2 (f, s) :: c) = Focus (s, LMap (fn y => f (v, y)) :: c)

fun locate (k, Focus (s, c)) =
   if first (k, s)
      then SOME (Focus (s, c))
   else if List.null c
      then NONE
   else
      case nullable s of
         NONE => NONE
       | SOME v => locate (k, plug (v, c))

fun pierce (k, Focus (s, c)) =
   case s of
      Map (f, x) => pierce (k, Focus (x, LMap f :: c))
    | LiftA2 (f, x, y) =>
         if first (k, x)
            then pierce (k, Focus (x, LLiftA2 (f, y) :: c))
            else
               let
                  val a = Option.valOf (nullable x)
               in
                  pierce (k, Focus (y, LMap (fn b => f (a, b)) :: c))
               end
    | Or (s1, s2) =>
         if first (k, s1)
            then pierce (k, Focus (s1, c))
            else pierce (k, Focus (s2, c))
    | Kind (f, _) => (fn t => Focus (Pure (f t), c))
    | Rec (_, f) => pierce (k, Focus (f (), c))
    | Empty => raise Fail ""
    | Pure _ => raise Fail ""

fun nullable' (Focus (s, c)) =
   case nullable s of
      NONE => NONE
    | SOME v =>
         if List.null c
            then SOME v
            else nullable' (plug (v, c))

val _: kind * focus -> focus option = locate
val _: exn * layer list -> focus = plug
val _: kind * focus -> token -> focus = pierce
val _: focus -> exn option = nullable'

(* val locate = fn args => (print "locate\n"; locate args) *)
(* val plug = fn args => (print "plug\n"; plug args) *)
(* val pierce = fn args => (print "pierce\n"; pierce args) *)
(* val nullable' = fn args => (print "nullable'\n"; nullable' args) *)

fun parsef (f, []) = nullable' f
  | parsef (f, t :: ts) =
   let
      val k = K (case t of T n => n)
   in
      case locate (k, f) of
         NONE => NONE
       | SOME f' => parsef (pierce (k, f') t, ts)
   end

fun parse (s, ts) =
   let
      exception E of 'a
      val s' = map E s
   in
      case parsef (Focus (s', []), List.map T ts) of
         NONE => NONE
       | SOME (E a) => SOME a
       | _ => raise Fail "parse: Impossible"
   end

end

(* vim: set tw=0 sw=3 ts=3: *)
