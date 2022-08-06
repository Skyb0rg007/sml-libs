
structure Parse: PARSE =
struct
   structure Tbl = HashTableFn(
      struct
         type hash_key = string
         val hashVal = FNVHash.hashString
         val sameKey: string * string -> bool = op =
      end)

   datatype matcher = Matcher of {
      input: string,
      pos: int ref,
      memoTable: result Tbl.hash_table array
   }

   and result = FAIL | SUCCESS of Universal.universal * int

   fun hasMemo (Matcher {memoTable, pos, ...}, ruleName) =
      Tbl.inDomain (Array.sub (memoTable, !pos)) ruleName

   (* fun memoize (Matcher {memoTable, pos, ...}, ruleName, res) = *)
   (*    let *)
   (*       val tag = Universal.tag () *)
   (*       val inj = Universal.tagInject tag *)
   (*       val prj = Universal.tagProject tag *)

   (*       val tbl = Array.sub (memoTable, !pos) *)
   (*    in *)
   (*       Tbl.insert tbl (ruleName, case res of *)
   (*                                     NONE => FAIL *)
   (*                                   | SOME x => SUCCESS (inj x, )) *)
   (*    end *)

   fun consume (Matcher {input, pos, ...}, c) =
      if String.sub (input, !pos) = c
         then (pos := !pos + 1; true)
      else false

   type 'a t = matcher -> 'a option

   fun terminal str matcher =
      let
         val len = String.size str
         fun go i =
            if i >= len 
               then SOME str
            else if consume (matcher, String.sub (str, i))
               then go (i + 1)
            else NONE
      in
         go 0
      end

   fun choice (a, b) (matcher as Matcher {pos, ...}) =
      let
         val origPos = !pos
      in
         case a matcher of
            SOME x => SOME (Either.INL x)
          | NONE => (pos := origPos; Option.map Either.INR (b matcher))
      end

   fun sequence (a, b) matcher =
      case a matcher of
         NONE => NONE
       | SOME x => Option.map (fn y => (x, y)) (b matcher)

   fun not a (matcher as Matcher {pos, ...}) =
      let
         val origPos = !pos
      in
         case a matcher of
            NONE => (pos := origPos; SOME ())
          | SOME _ => NONE
      end

   fun many a (matcher as Matcher {pos, ...}) =
      let
         val res = ref []
         fun go () =
            let
               val origPos = !pos
            in
               case a matcher of
                  NONE => (pos := origPos; SOME (List.rev (!res)))
                | SOME x => (res := x :: !res; go ())
            end
      in
         go ()
      end
end

(* vim: set tw=0 ts=3 sw=3: *)
