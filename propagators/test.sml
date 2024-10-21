
structure Test =
struct

structure C = Cell

structure Truth =
   struct
      datatype t =
         Unknown
       | True
       | False

      fun toString Unknown = "⊥"
        | toString True = "true"
        | toString False = "false"

      exception Inconsistent

      fun merge (Unknown, Unknown) = C.Unchanged
        | merge (Unknown, y) = C.Changed y
        | merge (_, Unknown) = C.Unchanged
        | merge (True, True) = C.Unchanged
        | merge (False, False) = C.Unchanged
        | merge (True, False) = raise Inconsistent
        | merge (False, True) = raise Inconsistent

      fun cell () = C.new Unknown merge

      fun and_ (a, b) =
         let
            val res = cell ()
         in
            (* a ∧ false = false
             * false ∧ b = false
             * true ∧ true = true *)
            C.watch2 a b
               (fn (_, False) => C.write (res, False)
                 | (False, _) => C.write (res, False)
                 | (True, True) => C.write (res, True)
                 | _ => ())
            (* a ∧ b = true ⇒ a = true ∧ b = true *)
            ; C.watch res
               (fn True => (C.write (a, True); C.write (b, True))
                 | _ => ())
            (* true ∧ b = false ⇒ b = false *)
            ; C.watch2 a res
               (fn (True, False) => C.write (b, False)
                 | _ => ())
            (* a ∧ true = false ⇒ a = false *)
            ; C.watch2 b res
               (fn (True, False) => C.write (a, False)
                 | _ => ())
            ; res
         end

   fun or_ (a, b) =
      let
         val res = cell ()
      in
         (* a ∨ true = true
          * true ∨ b = true
          * false ∨ false = false *)
         C.watch2 a b
            (fn (_, True) => C.write (res, True)
              | (True, _) => C.write (res, True)
              | (False, False) => C.write (res, False)
              | _ => ())
         (* a ∧ b = false ⇒ a = false ∧ b = false *)
         ; C.watch res
            (fn False => (C.write (a, False); C.write (b, False))
              | _ => ())
         (* false ∧ b = true ⇒ b = true *)
         ; C.watch2 a res
            (fn (False, True) => C.write (b, True)
              | _ => ())
         (* a ∧ false = true ⇒ a = true *)
         ; C.watch2 b res
            (fn (False, True) => C.write (a, True)
              | _ => ())
         ; res
      end

   end

fun testTruth () =
   let
      val b1 = Truth.cell ()
      val b2 = Truth.cell ()
      val b3 = Truth.cell ()
      val b4 = Truth.and_ (b1, Truth.or_ (b2, b3))
   in
      TextIO.print "\n=== No knowledge ===\n"
      ; TextIO.print ("b1 = " ^ Truth.toString (C.content b1) ^ "\n")
      ; TextIO.print ("b2 = " ^ Truth.toString (C.content b2) ^ "\n")
      ; TextIO.print ("b3 = " ^ Truth.toString (C.content b3) ^ "\n")
      ; TextIO.print ("b1 ∧ (b2 ∨ b3) = " ^ Truth.toString (C.content b4) ^ "\n")

      ; C.write (b1, Truth.True)

      ; TextIO.print "\n=== (b1 := true) ===\n"
      ; TextIO.print ("b1 = " ^ Truth.toString (C.content b1) ^ "\n")
      ; TextIO.print ("b2 = " ^ Truth.toString (C.content b2) ^ "\n")
      ; TextIO.print ("b3 = " ^ Truth.toString (C.content b3) ^ "\n")
      ; TextIO.print ("b1 ∧ (b2 ∨ b3) = " ^ Truth.toString (C.content b4) ^ "\n")

      ; C.write (b4, Truth.False)

      ; TextIO.print "\n=== (b1 ∧ (b2 ∨ b3) := false) ===\n"
      ; TextIO.print ("b1 = " ^ Truth.toString (C.content b1) ^ "\n")
      ; TextIO.print ("b2 = " ^ Truth.toString (C.content b2) ^ "\n")
      ; TextIO.print ("b3 = " ^ Truth.toString (C.content b3) ^ "\n")
      ; TextIO.print ("b1 ∧ (b2 ∨ b3) = " ^ Truth.toString (C.content b4) ^ "\n")
   end


structure Value =
   struct
      structure Set = RedBlackSetFn(
         struct
            type ord_key = string
            val compare = String.compare
         end)

      type t = Set.set

      fun toString v = "{" ^ String.concatWith "," (Set.listItems v) ^ "}"

      fun merge (v1, v2) =
         if Set.equal (v1, v2)
            then C.Unchanged
         else C.Changed (Set.union (v1, v2))

      fun cell () = C.new Set.empty merge

      fun add (cell, item) = C.write (cell, Set.singleton item)

      fun app k cell = C.watch cell (Set.app k)

      val content = Set.listItems o C.content
   end

fun testValue () =
   let
      val v1 = Value.cell ()
      val v2 = Value.cell ()
      val v3 = Value.cell ()
   in
      Value.app (fn x => Value.app (fn y => Value.add (v3, x ^ "-" ^ y)) v2) v1
      ; Value.add (v1, "foo")
      ; Value.add (v2, "quux")
      ; Value.add (v2, "asdf")
      ; Value.add (v1, "bar")
      ; Value.add (v1, "baz")

      ; TextIO.print "\n=== ===\n"
      ; TextIO.print ("v1 = " ^ Value.toString (C.content v1) ^ "\n")
      ; TextIO.print ("v2 = " ^ Value.toString (C.content v2) ^ "\n")
      ; TextIO.print ("v3 = " ^ Value.toString (C.content v3) ^ "\n")
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
