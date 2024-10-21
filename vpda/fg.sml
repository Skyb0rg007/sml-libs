
structure FG =
struct

datatype input =
   VAR of string
 | PLUS
 | TIMES

fun tokenize s =
   List.map
      (fn #"+" => PLUS
        | #"*" => TIMES
        | c =>
             if Char.isAlpha c
                then VAR (String.str c)
             else raise Fail "Invalid character")
      (String.explode s)

datatype precedence =
   EQ
 | LT
 | GT
 | NO

type matrix = input * input -> precedence

fun m1 (VAR _, VAR _) = NO
  | m1 (VAR _, PLUS) = GT
  | m1 (VAR _, TIMES) = GT
  | m1 (PLUS, VAR _) = LT
  | m1 (PLUS, PLUS) = GT
  | m1 (PLUS, TIMES) = LT
  | m1 (TIMES, VAR _) = LT
  | m1 (TIMES, PLUS) = GT
  | m1 (TIMES, TIMES) = GT


datatype state =
   DONE of bool
 | STATE of input list * input list

fun step _ ([], []) = DONE true
  | step _ ([], b :: input) = STATE ([b], input)
  | step m (a :: stack, []) =
         let
            fun loop (cur, a :: stack) =
               if m (cur, a) = GT
                  then (a :: stack)
               else loop (a, stack)
              | loop (_, []) = []
         in
            STATE (loop (a, stack), [])
         end
  | step m (a :: stack, b :: input) =
   case m (a, b) of
      NO => DONE false
    | EQ => STATE (b :: a :: stack, input)
    | LT => STATE (b :: a :: stack, input)
    | GT =>
         let
            fun loop (cur, a :: stack) =
               if m (cur, a) = GT
                  then (a :: stack)
               else loop (a, stack)
              | loop (_, []) = []
         in
            STATE (loop (a, stack), input)
         end

fun tosInput (VAR v) = "(VAR " ^ v ^ ")"
  | tosInput PLUS = "PLUS"
  | tosInput TIMES = "TIMES"

fun prState (stack, input) =
   (print ("stack = [" ^ String.concatWith "," (List.map tosInput stack) ^ "]\n");
    print ("input = [" ^ String.concatWith "," (List.map tosInput input) ^ "]\n\n"))

fun run m state =
   (prState state;
    case step m state of
       DONE r => r
     | STATE s => run m s)

(* fun run _ ([], []) = true *)
(*   | run _ (_ :: _, []) = false *)
(*   | run m ([], b :: input) = run m ([b], input) *)
(*   | run m (a :: stack, b :: input) = *)
(*    case m (a, b) of *)
(*       NO => false *)
(*     | EQ => run m (b :: a :: stack, input) *)
(*     | LT => run m (b :: a :: stack, input) *)
(*     | GT => *)
(*          let *)
(*             fun loop (cur, a :: stack) = *)
(*                if m (cur, a) = GT *)
(*                   then run m (a :: stack, input) *)
(*                else loop (a, stack) *)
(*               | loop (_, []) = false *)
(*          in *)
(*             loop (a, stack) *)
(*          end *)


fun test s = run m1 ([], tokenize s)

end

(* vim: set tw=0 ts=3 sw=3: *)
