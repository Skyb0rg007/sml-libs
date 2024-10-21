
structure Stride =
struct

datatype client = Client of {id: int, tickets: int, stride: int, pass: int ref}

val max_stride = Word.toInt (Word.<< (0w1, 0w20))
val counter = ref 0

local
   val clients = ref []

   fun cle (Client {pass = p1, ...}, Client {pass = p2, ...}) = !p1 <= !p2
in
   fun insert c =
      let
         fun go [] = [c]
           | go (c' :: cs) =
            if cle (c, c')
               then c :: c' :: cs
               else c' :: go cs
      in
         clients := go (!clients)
      end

   fun remove_min () =
      case !clients of
         [] => raise Subscript
       | c :: cs => (clients := cs; c)
end

fun new_client t =
   let
      val id = !counter
      val () = counter := !counter + 1
      val c = Client {
            id = id,
            tickets = t,
            stride = max_stride div t,
            pass = ref max_stride
         }
      val () = insert c
   in
      id
   end

fun allocate () =
   let
      val current = remove_min ()
      val Client {id, pass, stride, ...} = current
   in
      pass := !pass + stride;
      insert current;
      id
   end

fun test () =
   let
      val c1 = new_client 1000
      val c2 = new_client 4000
      val c1s = ref 0
      val c2s = ref 0

      fun loop 0 = ()
        | loop n =
         (if allocate () = c1
            then c1s := !c1s + 1
            else c2s := !c2s + 1;
          loop (n - 1))
   in
      loop 100000;
      print ("c1s: " ^ Int.toString (!c1s) ^ ", c2s = " ^ Int.toString (!c2s) ^ "\n")
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
