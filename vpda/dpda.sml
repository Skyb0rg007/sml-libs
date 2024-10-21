
structure DPDA =
struct

(* Deterministic Pushdown Automata *)
datatype ('X, 'Q, 'G) dpda = DPDA of {
      q0: 'Q,
      Z0: 'G,
      A: 'Q -> bool,
      delta: 'Q * 'G -> ('X, 'Q, 'G) res
   }

and ('X, 'Q, 'G) res =
   Eps of 'Q
 | Det of 'X -> 'Q * 'G list2

and 'a list2 =
   Nil
 | One of 'a
 | Two of 'a * 'a

fun runDPDA (DPDA {q0, Z0, A, delta}, input) =
   let
      fun go (state, _, []) = A state
        | go (state, [], _) = A state
        | go (state, tos :: stack, input) =
         case delta (state, tos) of
            Eps state' => go (state', stack, input)
          | Det f =>
               case input of
                  [] => A state
                | t :: ts =>
                     case f t of
                        (state', Nil) => go (state', stack, ts)
                      | (state', One a) => go (state', a :: stack, ts)
                      | (state', Two (a, b)) => go (state', a :: b :: stack, ts)
   in
      go (q0, [Z0], input)
   end


val bal = DPDA {
      q0 = true,
      Z0 = false,
      A = fn b => b,
      delta =
         fn (false, _) => Eps false
          | (true, true) => Det (fn #"(" => (true, Two (true, true))
                                  | #")" => (true, Nil)
                                  | #"." => (true, One true)
                                  | _ => (false, Nil))
          | (true, false) => Det (fn #"(" => (true, Two (true, false))
                                   | #"." => (false, Nil)
                                   | _ => (false, Nil))
   }

fun testBal s = runDPDA (bal, String.explode s)


(* Boolean series over W *)
structure B:
   sig
      type 'W t

      val + : 'W t * 'W t -> 'W t
      val * : 'W t * 'W t -> 'W t
      val zero: 'W t
      val one: 'W t
      val from: ''W list -> ''W t
      val support: 'W t -> 'W list -> bool
      val act: 'W t * 'W list -> 'W t
   end =
   struct
      type 'W t = 'W list -> bool

      fun (a + b) w = a w orelse b w
      fun (a * b) w = a w andalso b w
      fun zero _ = false
      fun one _ = true

      fun from w w' = w = w'
      fun support x = x
      fun act (s, w) w' = s (w @ w')
   end

(* Strict Deterministic Automata *)
datatype ('X, 'Q, 'G) sda = SDA of {
      q0: 'Q,
      Z0: 'G,
      A: 'Q -> bool,
      delta: 'Q * 'G * 'X -> ('Q * 'G list) list
   }

end

(* vim: set tw=0 ts=3 sw=3: *)
