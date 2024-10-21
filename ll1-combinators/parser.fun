
functor ParserFn(S: PARSER_STRUCTS): PARSER =
struct

structure Kind = S.Kind
structure Token = S.Token
structure IntSet = IntRedBlackSet
structure P = Propagator

type kind = Kind.t
type token = Token.t

datatype 'a t = T of {
      productive: P.Bool.t,
      nullable: 'a P.Option.t,
      first: P.Set.t,
      shouldNotFollow: P.Set.t,
      visitable: P.Set.t,
      node: 'a node
   }

and 'a node =
   Pure of 'a
 | Empty
 | Choice of 'a t * 'a t
 | Kind of (token -> 'a) * kind
 | Map2 of (exn * exn -> 'a) * exn t * exn t
 | Rec of int * (unit -> 'a t)

fun pure a = T {
      productive = P.Bool.true_,
      nullable = P.Option.some a,
      first = P.Set.empty,
      shouldNotFollow = P.Set.empty,
      visitable = P.Set.empty,
      node = Pure a
   }

val empty = T {
      productive = P.Bool.false_,
      nullable = P.Option.none,
      first = P.Set.empty,
      shouldNotFollow = P.Set.empty,
      visitable = P.Set.empty,
      node = Empty
   }

fun kind k = T {
      productive = P.Bool.true_,
      nullable = P.Option.none,
      first = P.Set.singleton (Kind.toInt k),
      shouldNotFollow = P.Set.empty,
      visitable = P.Set.empty,
      node = Kind (fn x => x, k)
   }

fun map f (T s) =
   if not (P.Bool.toBool (#productive s))
      then empty
      else
         T {
            productive = #productive s,
            nullable = P.Option.map f (#nullable s),
            first = #first s,
            shouldNotFollow = #shouldNotFollow s,
            visitable = #visitable s,
            node =
               case #node s of
                  Pure a => Pure (f a)
                | Empty => Empty
                | Kind (g, k) => Kind (f o g, k)
                | Map2 (g, x, y) => Map2 (f o g, x, y)
                | Choice (x, y) => Choice (map f x, map f y)
                | Rec (n, g) => Rec (n, map f o g)
         }

fun map2 f (T s1, T s2) =
   if not (P.Bool.toBool (#productive s1))
         orelse not (P.Bool.toBool (#productive s2))
      then empty
      else
         let
            exception A of 'a
            exception B of 'b
            fun f' (A a, B b) = f (a, b)
              | f' _ = raise Fail "Impossible"
         in
            T {
               productive = P.Bool.and_ (#productive s1, #productive s2),
               nullable = P.Option.map2 f (#nullable s1, #nullable s2),
               first =
                  P.Set.union
                     (P.Set.guard (#productive s2, #first s1),
                      P.Set.guard (P.Option.isSome (#nullable s1), #first s2)),
               shouldNotFollow =
                  P.Set.union
                     (P.Set.guard (P.Option.isSome (#nullable s2), #shouldNotFollow s1),
                      P.Set.guard (#productive s1, #shouldNotFollow s2)),
               visitable =
                  P.Set.union
                     (#visitable s1,
                      P.Set.guard (P.Option.isSome (#nullable s1), #visitable s2)),
               node = Map2 (f', map A (T s1), map B (T s2))
            }
         end

fun map3 f (s1, s2, s3) =
   map2 (fn ((a, b), c) => f (a, b, c))
      (map2 (fn (a, b) => (a,b)) (s1, s2),
       s3)

fun choice (T s1, T s2) =
   T {
      productive = P.Bool.or (#productive s1, #productive s2),
      nullable = P.Option.or (#nullable s1, #nullable s2),
      first = P.Set.union (#first s1, #first s2),
      shouldNotFollow =
         List.foldl P.Set.union P.Set.empty
         [ #shouldNotFollow s1,
           #shouldNotFollow s2,
           P.Set.guard (P.Option.isSome (#nullable s2), #first s1),
           P.Set.guard (P.Option.isSome (#nullable s1), #first s2)],
      visitable = P.Set.union (#visitable s1, #visitable s2),
      node = Choice (T s1, T s2)
   }

val counter = ref 0

fun fix f =
   let
      val id = !counter
      val () = counter := !counter + 1
      val productive = P.Bool.newCell ()
      val nullable = P.Option.newCell ()
      val first = P.Set.newCell ()
      val shouldNotFollow = P.Set.newCell ()
      val visitable = P.Set.newCell ()

      val self = ref NONE

      val input = T {
            productive = P.Bool.cell productive,
            nullable = P.Option.cell nullable,
            first = P.Set.cell first,
            shouldNotFollow = P.Set.cell shouldNotFollow,
            visitable = P.Set.cell visitable,
            node = Rec (id, fn () => Option.valOf (!self))
         }

      val T syn = f input
   in
      self := SOME (T syn);
      P.Bool.setCell (productive, #productive syn);
      P.Option.setCell (nullable, #nullable syn);
      P.Set.setCell (first, #first syn);
      P.Set.setCell (shouldNotFollow, #shouldNotFollow syn);
      P.Set.setCell (visitable, #visitable syn);
      T syn
   end

datatype layer =
   LMap of exn -> exn
 | LMap2 of (exn * exn -> exn) * exn t

datatype focus = Focus of exn t * layer list

fun plug (v, []) = Focus (pure v, [])
  | plug (v, LMap f :: c) = plug (f v, c)
  | plug (v, LMap2 (f, s) :: c) = Focus (s, LMap (fn y => f (v, y)) :: c)

fun fparse (focus, t :: ts) =
   let
      val k = Token.kind t
      val n = Kind.toInt k

      val () = print ("Recognizing token with kind " ^ Int.toString n ^ "\n")

      fun pierce (Focus (T {node, ...}, c), t) =
         case node of
            Map2 (f, x as T {nullable, first, ...}, y) =>
               if IntSet.member (P.Set.toSet first, n)
                  then pierce (Focus (x, LMap2 (f, y) :: c), t)
                  else
                     let
                        val a = Option.valOf (P.Option.toOption nullable)
                     in
                        pierce (Focus (y, LMap (fn b => f (a, b)) :: c), t)
                     end
          | Choice (x as T {first, ...}, y) =>
               if IntSet.member (P.Set.toSet first, n)
                  then pierce (Focus (x, c), t)
                  else pierce (Focus (y, c), t)
          | Kind (f, _) => Focus (pure (f t), c)
          | Rec (_, f) => pierce (Focus (f (), c), t)
          (* Prerequisite is that syntax matched token t *)
          | Empty => raise Fail "pierce: impossible"
          | Pure _ => raise Fail "pierce: impossible"

      fun go (f as Focus (T {first, nullable, ...}, c)) =
         let
         in
            print ("first = [" ^ String.concatWith "," (List.map Int.toString (IntSet.listItems (P.Set.toSet first))) ^ "]\n");
         if IntSet.member (P.Set.toSet first, n)
            then fparse (pierce (f, t), ts)
         else if List.null c
            then NONE
         else
            case P.Option.toOption nullable of
               NONE => NONE
             | SOME v => go (plug (v, c))
         end
   in
      go focus
   end
  | fparse (focus, []) =
   let
      fun go (Focus (T {nullable, ...}, c)) =
         case P.Option.toOption nullable of
            NONE => NONE
          | SOME v => if List.null c then SOME v else go (plug (v, c))
   in
      go focus
   end

(* fun isLL1 (T syn) = *)
(*    P.Option.is *)

fun parse (syn, toks) =
   let
      exception E of 'a
      val focus = Focus (map E syn, [])
   in
      case fparse (focus, toks) of
         NONE => NONE
       | SOME (E v) => SOME v
       | SOME _ => raise Fail "Impossible"
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
