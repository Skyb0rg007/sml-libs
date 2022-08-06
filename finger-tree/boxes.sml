
structure BoxPP =
(* sig *)
(*    type box *)

(*    datatype alignment = *)
(*       First *)
(*     | Center1 *)
(*     | Center2 *)
(*     | Last *)

(*    val nullBox: box *)
(*    val char: char -> box *)
(*    val string: string -> box *)

(*    val moveUp: int * box -> box *)
(*    val moveDown: int * box -> box *)
(*    val moveLeft: int * box -> box *)
(*    val moveRight: int * box -> box *)
(* end = *)
struct
   datatype alignment =
      First
    | Center1
    | Center2
    | Last

   datatype content =
      Blank
    | Text of string
    | Row of box list
    | Col of box list
    | Sub of alignment * alignment * box

   withtype box = {rows: int, cols: int, content: content}


   fun emptyBox (r, c) = {rows = r, cols = c, content = Blank}

   val nullBox = emptyBox (0, 0)

   fun char c = {rows = 1, cols = 1, content = Text (String.str c)}

   fun string s = {rows = 1, cols = String.size s, content = Text s}

   fun align (ah, av, r, c, b) = {rows = r, cols = c, content = Sub (ah, av, b)}

   fun alignHoriz (a, c, b) = align (a, First, #rows b, c, b)

   fun alignVert (a, r, b) = align (First, a, r, #cols b, b)

   fun moveUp (n, b) = alignVert (First, #rows b + n, b)

   fun moveDown (n, b) = alignVert (Last, #rows b + n, b)

   fun moveLeft (n, b) = alignHoriz (First, #cols b + n, b)

   fun moveRight (n, b) = alignHoriz (Last, #cols b + n, b)

   fun hcat (a, bs) =
      ()

   structure Foldl':
   sig
      type ('a, 'b) t

      (* Construction *)
      val Fold: ('a * 'x -> 'x) -> 'x -> ('x -> 'b) -> ('a, 'b) t

      val foldList: ('a, 'b) t -> 'a list -> 'b

      val lmap: ('a2 -> 'a1) -> ('a1, 'b) t -> ('a2, 'b) t
      val rmap: ('b1 -> 'b2) -> ('a, 'b1) t -> ('a, 'b2) t

      val head: unit -> ('a, 'a option) t
      val drop: int -> ('a, 'b) t -> ('a, 'b) t
   end =
   struct
      open Universal

      datatype ('a, 'b) t = T of {
         init: universal,
         step: 'a * universal -> universal,
         done: universal -> 'b
      }

      fun Fold step init done =
         let
            val t = tag ()
         in
            T {
               init = tagInject t init,
               step = fn (a, x) => tagInject t (step (a, tagProject t x)),
               done = fn x => done (tagProject t x)
            }
         end

      fun foldList (T {init, step, done}) xs =
         let
            fun cons (a, k) x = k (step (a, x))
         in
            List.foldr cons done xs init
         end

      fun lmap f (T {init, step, done}) =
         T {
            init = init,
            step = fn (a, x) => step (f a, x),
            done = done
         }

      fun rmap f (T {init, step, done}) =
         T {
            init = init,
            step = step,
            done = fn x => f (done x)
         }

      fun head () =
         let
            val t = tag ()
         in
            T {
               init = tagInject t NONE,
               step = fn (a, x) =>
                  case tagProject t x of
                     NONE => tagInject t (SOME a)
                   | SOME y => tagInject t (SOME y),
               done = tagProject t
            }
         end

      fun drop n (T {init, step, done}) =
         let
            val t = tag ()
         in
            T {
               init = tagInject t (n, init),
               step = fn (a, x) =>
                  case tagProject t x of
                     (0, y) => tagInject t (0, step (a, y))
                   | (n, y) => tagInject t (n - 1, y),
               done = fn x => done (#2 (tagProject t x))
            }
         end
   end

   structure Foldl:
   sig
      type ('a, 'b, 'x) t
      type ('a, 'b) either = ('a, 'b) Either.either

      (* foldl is a profunctor *)
      val lmap: ('a2 -> 'a1) -> ('a1, 'b, 'x) t -> ('a2, 'b, 'x) t
      val rmap: ('b1 -> 'b2) -> ('a, 'b1, 'x) t -> ('a, 'b2, 'x) t

      (* foldl is a choice profunctor *)
      val left: ('a, 'b, 'x) t -> (('a, 'c) either, ('b, 'c) either, ('x, 'c) either) t
      val right: ('a, 'b, 'x) t -> (('c, 'a) either, ('c, 'b) either, ('c, 'x) either) t

      (* foldl is a semigroupoid *)
      val o: ('b, 'c, 'x) t * ('a, 'b, 'y) t -> ('a, 'c, 'x * 'y) t
   end =
   struct
      datatype either = datatype Either.either

      datatype ('a, 'b, 'x) t = T of {
         init: 'x,
         step: 'a * 'x -> 'x,
         extract: 'x -> 'b
      }

      fun lmap f (T {init, step, extract}) =
         T {
            init = init,
            step = fn (a, x) => step (f a, x),
            extract = extract
         }

      fun rmap f (T {init, step, extract}) =
         T {
            init = init,
            step = step,
            extract = fn x => f (extract x)
         }

      fun left (T {init, step, extract}) =
         T {
            init = INL init,
            step = fn (INL a, INL x) => INL (step (a, x))
                    | (_, INR c) => INR c
                    | (INR c, _) => INR c,
            extract = Either.mapLeft extract
         }

      fun right (T {init, step, extract}) =
         T {
            init = INR init,
            step = fn (INR a, INR x) => INR (step (a, x))
                    | (_, INL c) => INL c
                    | (INL c, _) => INL c,
            extract = Either.mapRight extract
         }

      fun (T {init = i1, step = s1, extract = e1}) o (T {init = i2, step = s2, extract = e2}) =
         T {
            init = (i1, i2),
            step = fn (a, (x, y)) =>
               let
                  val y' = s2 (a, y)
                  val x' = s1 (e2 y', x)
               in
                  (x', y')
               end,
            extract = fn (x, _) => e1 x
         }
   end
end
(* vim: set ft=sml tw=0 ts=3 sw=3: *)
