
structure SExpPrint =
struct
   structure H =
   struct

   end

   type size = {sum: int, max: int}

   datatype indent =
      Swing
    | SwingAfter of int
    | Align

   datatype ir =
      IATOM of string
    | IEMPTY
    | ILIST of indent * ir * ir list * string option
    | IVECT of ir * ir list

   (* fun sizeof IEMPTY = {sum = 2, max = 2} *)
   (*   | sizeof (IATOM s) = {sum = String.size s, max = String.size s} *)
   (*   | sizeof (ILIST (_, s, _, _, _)) = s *)
   (*   | sizeof (IVECT (_, s, _, _)) = s *)

   (* fun addSize ({sum = s1, max = m1}, {sum = s2, max = m2}) = *)
   (*    {sum = s1 + s2 + 1, max = Int.max (m1, m2)} *)

   fun isSeparator c =
      case c of
         #"\t" => true
       | #"\n" => true
       | #"\v" => true
       | #"\f" => true
       | #"\r" => true
       | #" " => true
       | #"\"" => true
       | #"'" => true
       | #"(" => true
       | #")" => true
       | #"," => true
       | #";" => true
       | #"[" => true
       | #"]" => true
       | #"{" => true
       | #"}" => true
       | _ => false

   fun toIR swing sexp =
      let
         fun headOf SExp.NIL = IEMPTY
           | headOf (SExp.CONS (x, xs)) = gather (swing x, headOf x, [], xs)
           | headOf (SExp.VECTOR v) =
            (case map headOf (Vector.toList v) of
                [] => IATOM "#()"
              | x::xs => IVECT (x, xs))
           | headOf atom = IATOM (SExp.toString atom)

         and gather (sw, hd, rs, SExp.CONS (x, xs)) = gather (sw, hd, headOf x :: rs, xs)
           | gather (sw, hd, rs, SExp.NIL) = ILIST (sw, hd, List.rev rs, NONE)
           | gather (sw, hd, rs, atom) = ILIST (sw, hd, List.rev rs, SOME (SExp.toString atom))
      in
         headOf sexp
      end


   fun printIR ir =
      let
         fun isBasic (IATOM _) = true
           | isBasic IEMPTY = true
           | isBasic _ = false

         fun ppBasic (IATOM a) = a
           | ppBasic IEMPTY = "()"
           | ppBasic _ = raise Fail "Not basic"

         fun pTail NONE = ")"
           | pTail (SOME t) = " . " ^ t ^ ")"

         fun addOpen [] = ["("]
           | addOpen (x::xs) = "(" ^ x :: xs

         fun addVOpen [] = ["#("]
           | addVOpen (x::xs) = "#(" ^ x :: xs

         fun addClose [] = [")"]
           | addClose xs =
            let
               val len = List.length xs
               fun f (i, v) =
                  if i = len - 1
                     then v ^ ")"
                  else v
            in
               List.mapi f xs
            end

         fun indent n s = CharVector.tabulate (n, fn _ => #" ") ^ s

         fun handleTail NONE xs = addClose xs
           | handleTail (SOME t) xs = xs @ [" . " ^ t ^ ")"]

         fun maximum [] = raise Fail "Empty list!"
           | maximum (x::xs) = foldr Int.max x xs

         fun go (IATOM t) = [t]
           | go IEMPTY = ["()"]
           | go (IVECT (x, xs)) =
            if List.all isBasic (x :: xs)
               then ["#(" ^ String.concatWith " " (List.map ppBasic (x :: xs)) ^ ")"]
            else addVOpen (go x) @ addClose (List.map (indent 2) (List.concatMap go xs))
           | go (ILIST (iv, x, xs, rest)) =
            if List.all isBasic (x :: xs)
               then ["(" ^ String.concatWith " " (List.map ppBasic (x :: xs)) ^ pTail rest]
            else
               case iv of
                  Swing =>
                     let
                        val butLast = addOpen (go x) @ List.map (indent 2) (List.concatMap go xs)
                     in
                        handleTail rest butLast
                     end
                | SwingAfter n =>
                     let
                        val (hs, xs) = List.splitAt (x :: xs, n)
                        val hd = "(" ^ String.concatWith " " (List.concatMap go hs)
                        val butLast = hd :: List.map (indent 2) (List.concatMap go xs)
                     in
                        handleTail rest butLast
                     end
                | Align =>
                     let
                        val len = 2 + maximum (List.map String.size (go x))
                     in
                        case xs of
                           [] => addOpen (addClose (go x))
                         | y::ys =>
                              let
                                 val hd = "(" ^ String.concatWith " " (List.concatMap go [x, y])
                                 val butLast = hd :: List.map (indent len) (List.concatMap go ys)
                              in
                                 handleTail rest butLast
                              end
                     end
      in
         String.concatWith "\n" (go ir)
      end

   fun pprintWith spec sexp = printIR (toIR spec sexp)

   val pprint = pprintWith (fn _ => Swing)
end

(* vim: set ts=3 sw=3 tw=0: *)
