
structure Test =
struct

structure Input =
   struct
      datatype call =
         LParen
       | LBrack

      datatype return =
         RParen
       | RBrack

      datatype internal =
         Dot

      fun tokenize (c, r, i) s =
         List.map
            (fn #"(" => c LParen
              | #"[" => c LBrack
              | #")" => r RParen
              | #"]" => r RBrack
              | #"." => i Dot
              | _ => raise Fail "Invalid char")
            (String.explode s)

      fun tosCall LParen = "LParen"
        | tosCall LBrack = "LBrack"

      fun tosReturn RParen = "RParen"
        | tosReturn RBrack = "RBrack"

      fun tosInt Dot = "Dot"
   end

structure L1 =
   struct
      structure Input = Input

      type state = bool

      datatype stack =
         Paren
       | Brack

      fun tosStack Paren = "Paren"
        | tosStack Brack = "Brack"

      val start_state = true

      val all_states = [true, false]

      fun accepting b = b

      fun transition_call (true, Input.LParen) = (true, Paren)
        | transition_call (true, Input.LBrack) = (true, Brack)
        | transition_call (false, _) = (false, Paren)

      fun transition_return (true, Input.RParen, SOME Paren) = true
        | transition_return (true, Input.RBrack, SOME Brack) = true
        | transition_return _ = false

      fun transition_internal (b, Input.Dot) = b
   end

structure L2 =
   struct
      structure Input = Input

      type state = bool

      datatype stack =
         Paren
       | Brack

      fun tosStack Paren = "Paren"
        | tosStack Brack = "Brack"

      val start_state = true

      val all_states = [true, false]

      fun accepting b = b

      fun transition_call (true, Input.LParen) = (true, Paren)
        | transition_call (true, Input.LBrack) = (true, Brack)
        | transition_call (false, _) = (false, Paren)

      fun transition_return (true, Input.RParen, SOME Brack) = true
        | transition_return (true, Input.RBrack, SOME Paren) = true
        | transition_return _ = false

      fun transition_internal (b, Input.Dot) = b
   end

structure U = VPDA_Union(struct structure M1 = L1 structure M2 = L2 end)
structure I = VPDA_Intersection(struct structure M1 = L1 structure M2 = L2 end)

structure A1 = RunVPDA(L1)
structure A2 = RunVPDA(L2)
structure AU = RunVPDA(U)
structure AI = RunVPDA(I)

val run1 = A1.runTraced Bool.toString L1.tosStack (fn A1.Call c => Input.tosCall c | A1.Return r => Input.tosReturn r | A1.Internal a => Input.tosInt a) o Input.tokenize (A1.Call, A1.Return, A1.Internal)
val run2 = A2.runTraced Bool.toString L2.tosStack (fn A2.Call c => Input.tosCall c | A2.Return r => Input.tosReturn r | A2.Internal a => Input.tosInt a) o Input.tokenize (A2.Call, A2.Return, A2.Internal)

fun tosUState (s, r) =
   let
      fun f (Either.INL b) = "INL " ^ Bool.toString b
        | f (Either.INR b) = "INR " ^ Bool.toString b

      fun g NONE = "∅"
        | g (SOME s') = f s'

      val slt = g (s (Either.INL true))
      val slf = g (s (Either.INL false))
      val srt = g (s (Either.INR true))
      val srf = g (s (Either.INR false))
   in
      "({ INL true => " ^ slt ^ ", INL false => " ^ slf ^ ", INR true => " ^ srt ^ ", INR false => " ^ srf
      ^ "}, ["
      ^ String.concatWith "," (List.map f r) ^ "])"
   end

fun tosUStack (s, a) =
   "(" ^ tosUState s ^ "," ^ Input.tosCall a ^ ")"

val runu = AU.runTraced tosUState tosUStack (fn AU.Call c => Input.tosCall c | AU.Return r => Input.tosReturn r | AU.Internal a => Input.tosInt a) o Input.tokenize (AU.Call, AU.Return, AU.Internal)
(* val runu = AU.run o Input.tokenize (AU.Call, AU.Return, AU.Internal) *)
val runi = AI.run o Input.tokenize (AI.Call, AI.Return, AI.Internal)

fun testInput s =
   let
      val r1 = run1 s
      val r2 = run2 s
      val ru = runu s
      val ri = runi s

      fun assert (true, _) = ()
        | assert (false, msg) = print ("Error on input " ^ s ^ " with " ^ msg ^ "\n")
   in
      assert ((r1 andalso r2) = ri, "intersection");
      assert ((r1 orelse r2) = ru, "union")
   end

val rng = Random.rand (100, 200)

fun generateChar () =
   case Random.randRange (0, 4) rng of
      0 => #"["
    | 1 => #"("
    | 2 => #"]"
    | 3 => #")"
    | 4 => #"."
    | _ => raise Fail "impossible"

fun generate () =
   CharVector.tabulate (Random.randRange (1, 3000) rng, fn _ => generateChar ())

fun test n =
   ignore (List.tabulate (n, fn _ => testInput (generate ())))
end

(* vim: set tw=0 ts=3 sw=3: *)
