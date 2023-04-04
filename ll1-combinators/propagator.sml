
structure Propagator: PROPAGATOR =
struct

exception Inconsistent

(* false < true *)
structure Bool =
   struct
      datatype t =
         True
       | False
       | Cell of cell

      withtype cell = {
         callbacks: (unit -> unit) list ref,
         value: bool ref
      }

      val true_ = True
      val false_ = False
      val cell = Cell

      fun toBool True = true
        | toBool False = false
        | toBool (Cell {value, ...}) = !value

      fun newCell () = {callbacks = ref [], value = ref false}

      fun setTrue {value = ref true, ...} = ()
        | setTrue {value as ref false, callbacks as ref cbs} =
         (value := true;
          callbacks := [];
          List.app (fn k => k ()) cbs)

      fun setCell ({value = ref true, ...}, _) = raise Fail "Cell set more than once"
        | setCell (_, False) = ()
        | setCell (c, True) = setTrue c
        | setCell (c, Cell {value = ref true, ...}) = setTrue c
        | setCell (c, Cell {value = ref false, callbacks}) =
         callbacks := (fn () => if !(#value c) then () else setTrue c) :: !callbacks

      fun and_ (False, _) = False
        | and_ (_, False) = False
        | and_ (True, b) = b
        | and_ (b, True) = b
        | and_ (Cell {value = ref true, ...}, b) = b
        | and_ (b, Cell {value = ref true, ...}) = b
        | and_ (Cell c1, Cell c2) =
         let
            val {callbacks = cbs1, value = b1} = c1
            val {callbacks = cbs2, value = b2} = c2
            val cbs = ref []
            val b = ref false
            fun update () =
               if !b1 andalso !b2
                  then (b := true; List.app (fn k => k ()) (!cbs); cbs := [])
                  else ()
         in
            cbs1 := update :: !cbs1;
            cbs2 := update :: !cbs2;
            Cell {callbacks = cbs, value = b}
         end

      fun or (False, b) = b
        | or (b, False) = b
        | or (True, _) = True
        | or (_, True) = True
        | or (Cell {value = ref true, ...}, _) = True
        | or (_, Cell {value = ref true, ...}) = True
        | or (Cell c1, Cell c2) =
         let
            val {callbacks = cbs1, value = b1} = c1
            val {callbacks = cbs2, value = b2} = c2
            val cbs = ref []
            val b = ref false
            fun update () =
               if !b
                  then ()
                  else
                     (b := true;
                      List.app (fn k => k ()) (!cbs);
                      cbs := [])
         in
            cbs1 := update :: !cbs1;
            cbs2 := update :: !cbs2;
            Cell {callbacks = cbs, value = b}
         end
   end

(* NONE < SOME x < Τ *)
structure Option =
   struct
      datatype 'a value =
         VNONE
       | VSOME of 'a
       | VTOP

      datatype 'a t =
         None
       | Some of 'a
       | Top
       | Cell of 'a cell

      withtype 'a cell = {
         callbacks: (unit -> unit) list ref,
         value: 'a value ref
      }

      val none = None
      val some = Some
      val cell = Cell

      fun newCell () = {callbacks = ref [], value = ref VNONE}

      fun setTop {value, callbacks} =
         case !value of
            VTOP => ()
          | _ => (value := VTOP; List.app (fn k => k ()) (!callbacks); callbacks := [])

      fun setSome (c as {value, callbacks}, x) =
         case !value of
            VTOP => ()
          | VSOME _ => setTop c
          | VNONE => (value := VSOME x; List.app (fn k => k ()) (!callbacks); callbacks := [])

      fun setCell ({value = ref VTOP, ...}, _) = raise Fail "Cell set more than once"
        | setCell ({value = ref (VSOME _), ...}, _) = raise Fail "Cell set more than once"
        | setCell (_, None) = ()
        | setCell (c, Some x) = setSome (c, x)
        | setCell (c, Top) = setTop c
        | setCell (c, Cell {value = ref VTOP, ...}) = setTop c
        | setCell (c, Cell {value, callbacks}) =
         let
            fun update () =
               case !value of
                  VNONE => ()
                | VSOME x => setSome (c, x)
                | VTOP => setTop c
         in
            callbacks := update :: !callbacks;
            update ()
         end

      fun toOption None = NONE
        | toOption (Some x) = SOME x
        | toOption Top = raise Inconsistent
        | toOption (Cell {value, ...}) =
         case !value of
            VNONE => NONE
          | VSOME x => SOME x
          | VTOP => raise Inconsistent

      fun inconsistent Top = true
        | inconsistent (Cell {value = ref VTOP, ...}) = true
        | inconsistent _ = false

      fun or (Top, _) = Top
        | or (_, Top) = Top
        | or (None, x) = x
        | or (x, None) = x
        | or (Some _, Some _) = Top
        | or (Some _, Cell {value = ref VTOP, ...}) = Top
        | or (Cell {value = ref VTOP, ...}, Some _) = Top
        | or (Some _, Cell {value = ref (VSOME _), ...}) = Top
        | or (Cell {value = ref (VSOME _), ...}, Some _) = Top
        | or (Cell {value = ref (VSOME _), ...}, Cell {value = ref (VSOME _), ...}) = Top
        | or (Cell {callbacks, value = ref VNONE}, Some x) =
         let
            val cbs = ref []
            val opt = ref (VSOME x)
            fun set _ =
               (opt := VTOP;
                List.app (fn k => k ()) (!cbs);
                cbs := [])
         in
            callbacks := set :: !callbacks;
            Cell {callbacks = cbs, value = opt}
         end
        | or (Some x, Cell {callbacks, value = ref VNONE}) =
         let
            val cbs = ref []
            val opt = ref (VSOME x)
            fun set _ =
               (opt := VTOP;
                List.app (fn k => k ()) (!cbs);
                cbs := [])
         in
            callbacks := set :: !callbacks;
            Cell {callbacks = cbs, value = opt}
         end
        | or (Cell {callbacks = cb1, value = opt1}, Cell {callbacks = cb2, value = opt2}) =
         let
            val cbs = ref []
            val opt = ref VNONE
            fun setTop () =
               case !opt of
                  VTOP => ()
                | _ => (opt := VTOP; List.app (fn k => k ()) (!cbs); cbs := [])

            fun setSome x =
               case !opt of
                  VNONE => (opt := VSOME x; List.app (fn k => k ()) (!cbs))
                | _ => setTop ()

            fun update () =
               case (!opt1, !opt2) of
                  (VTOP, _) => setTop ()
                | (_, VTOP) => setTop ()
                | (VSOME x, VSOME y) => setTop ()
                | (VSOME x, VNONE) => setSome x
                | (VNONE, VSOME x) => setSome x
                | (VNONE, VNONE) => ()
         in
            cb1 := update :: !cb1;
            cb2 := update :: !cb2;
            Cell {callbacks = cbs, value = opt}
         end

      fun map _ None = None
        | map f (Some x) = Some (f x)
        | map _ Top = Top
        | map _ (Cell {value = ref VTOP, ...}) = Top
        | map f (Cell {callbacks, value}) =
         let
            val cbs = ref []
            val opt = ref
               (case !value of
                   VNONE => VNONE
                 | VSOME x => VSOME (f x)
                 | VTOP => raise Fail "impossible")

            fun update () =
               case !value of
                  VNONE => raise Fail "impossible"
                | VSOME x => (opt := VSOME (f x); List.app (fn k => k ()) (!cbs))
                | VTOP => (opt := VTOP; List.app (fn k => ()) (!cbs); cbs := [])
         in
            callbacks := update :: !callbacks;
            Cell {callbacks = cbs, value = opt}
         end

      fun map2 _ (Top, _) = Top
        | map2 _ (_, Top) = Top
        | map2 _ (Cell {value = ref VTOP, ...}, _) = Top
        | map2 _ (_, Cell {value = ref VTOP, ...}) = Top
        | map2 _ (None, _) = None
        | map2 _ (_, None) = None
        | map2 f (Some x, Some y) = Some (f (x, y))
        | map2 f (Some x, c as Cell _) = map (fn y => f (x, y)) c
        | map2 f (c as Cell _, Some y) = map (fn x => f (x, y)) c
        | map2 f (Cell c1, Cell c2) =
         let
            val {callbacks = cb1, value = opt1} = c1
            val {callbacks = cb2, value = opt2} = c2
            val cbs = ref []
            val opt =
               case (!opt1, !opt2) of
                  (VSOME x, VSOME y) => ref (VSOME (f (x, y)))
                | _ => ref VNONE

            fun setTop () =
               case !opt of
                  VTOP => ()
                | _ => (opt := VTOP; List.app (fn k => k ()) (!cbs); cbs := [])

            fun update () =
               case (!opt1, !opt2) of
                  (VTOP, _) => setTop ()
                | (_, VTOP) => setTop ()
                | (VNONE, VNONE) => raise Fail "impossible"
                | (VSOME _, VNONE) => ()
                | (VNONE, VSOME _) => ()
                | (VSOME x, VSOME y) => (opt := VSOME (f (x, y)); List.app (fn k => k ()) (!cbs))
         in
            cb1 := update :: !cb1;
            cb2 := update :: !cb2;
            Cell {callbacks = cbs, value = opt}
         end
   end

structure Set =
   struct
      structure S = IntRedBlackSet

      datatype value = TOP | SET of S.set

      datatype t =
         Set of S.set
       | Top
       | Cell of cell

      withtype cell = {callbacks: (value -> unit) list ref, value: value ref}

      val empty = Set S.empty
      fun singleton x = Set (S.singleton x)
      val cell = Cell

      fun inconsistent Top = true
        | inconsistent (Cell {value = ref TOP, ...}) = true
        | inconsistent _ = false

      fun toSet (Set s) = s
        | toSet (Cell {value = ref (SET s), ...}) = s
        | toSet _ = raise Inconsistent

      fun newCell () = {callbacks = ref [], value = ref (SET S.empty)}

      fun setTop {value = ref TOP, ...} = ()
        | setTop {value, callbacks as ref cbs} =
         (value := TOP;
          callbacks := [];
          List.app (fn k => k TOP) cbs)

      fun setSet ({value = ref TOP, ...}, _) = ()
        | setSet ({value as ref (SET s1), callbacks = ref cbs}, s2) =
         let
            val diff = S.difference (s1, s2)
         in
            if S.isEmpty diff
               then ()
               else
                  (value := SET (S.union (s1, diff));
                   List.app (fn k => k (SET diff)) cbs)
         end

      fun setCell ({value = ref TOP, ...}, _) = raise Fail "Cell set more than once"
        | setCell (c, Top) = setTop c
        | setCell (c, Set s) = setSet (c, s)
        | setCell (c, Cell {value = ref TOP, ...}) = setTop c
        | setCell (c, Cell {value, callbacks}) =
         let
            fun update TOP = setTop c
              | update (SET s) = setSet (c, s)
         in
            callbacks := update :: !callbacks;
            update (!value)
         end

      fun union (Top, _) = Top
        | union (_, Top) = Top
        | union (Cell {value = ref TOP, ...}, _) = Top
        | union (_, Cell {value = ref TOP, ...}) = Top
        | union (Set a, Set b) = Set (S.union (a, b))
        | union (Set a, Cell {value, callbacks}) =
         let
            val callbacks = ref []
            val value = ref (SET S.empty)
            val c = {callbacks = callbacks, value = value}

            fun update TOP = setTop c
              | update (SET s) = setSet (c, s)
         in
            callbacks := update :: !callbacks;
            update (SET a);
            Cell c
         end
        | union (Cell {value, callbacks}, Set a) =
         let
            val callbacks = ref []
            val value = ref (SET S.empty)
            val c = {callbacks = callbacks, value = value}

            fun update TOP = setTop c
              | update (SET s) = setSet (c, s)
         in
            callbacks := update :: !callbacks;
            update (SET a);
            Cell c
         end
        | union (Cell c1, Cell c2) =
         let
            val {callbacks = cbs1, value = s1} = c1
            val {callbacks = cbs2, value = s2} = c1
            val callbacks = ref []
            val value = ref (SET S.empty)
            val c = {callbacks = callbacks, value = value}

            fun update TOP = setTop c
              | update (SET s) = setSet (c, s)
         in
            cbs1 := update :: !cbs1;
            cbs2 := update :: !cbs2;
            update (!s1);
            update (!s2);
            Cell c
         end

      fun guardConst (cbs, const) =
         let
            val value = ref (SET S.empty)
            val callbacks = ref []
            fun update () =
               (value := const;
                List.app (fn k => k const) (!callbacks);
                callbacks := [])
         in
            cbs := update :: !cbs;
            Cell {value = value, callbacks = callbacks}
         end

      fun guard (Bool.True, a) = a
        | guard (Bool.False, _) = Set S.empty
        | guard (Bool.Cell {value = ref true, ...}, a) = a
        | guard (Bool.Cell {value = ref false, callbacks}, Top) =
         guardConst (callbacks, TOP)
        | guard (Bool.Cell {value = ref false, callbacks}, Set s) =
         if S.isEmpty s
            then Set S.empty
            else guardConst (callbacks, SET s)
        | guard (Bool.Cell {value = v1 as ref false, callbacks = cbs1}, Cell {value = v2, callbacks = cbs2}) =
         let
            val reservoir = ref (SET S.empty)
            val value = ref (SET S.empty)
            val callbacks = ref []
            val c = {callbacks = callbacks, value = value}

            fun update1 () =
               case !reservoir of
                  TOP => setTop c
                | SET s => setSet (c, s)

            fun merge (TOP, _) = TOP
              | merge (_, TOP) = TOP
              | merge (SET s1, SET s2) = SET (S.union (s1, s2))

            fun update2 delta =
               if !v1
                  then
                     case delta of
                        TOP => setTop c
                      | SET s => setSet (c, s)
                  else (reservoir := merge (!reservoir, delta))
         in
            cbs1 := update1 :: !cbs1;
            cbs2 := update2 :: !cbs2;
            Cell c
         end
   end
end

(* vim: set tw=0 sw=3 ts=3: *)
