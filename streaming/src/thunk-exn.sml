
structure ThunkExn :> STREAM =
struct

type 'a t = unit -> 'a

exception Stop

fun unfold f seed =
   let
      val seed = ref seed
   in
      fn () =>
         case f (!seed) of
            NONE => raise Stop
          | SOME (x, seed') => (seed := seed'; x)
   end

fun map f gen () = f (gen ())

fun filter p gen () =
   let
      val x = gen ()
   in
      if p x then x else filter p gen ()
   end

fun take n gen =
   let
      val i = ref 0
   in
      fn () =>
         if !i = n
            then raise Stop
            else (i := !i + 1; gen ())
   end

fun fold f acc gen =
   case (SOME (gen ()) handle Stop => NONE) of
      NONE => acc
    | SOME x => fold f (f (x, acc)) gen
   (* fold f (f (gen (), acc)) gen *)
   (* handle Stop => acc *)

datatype 'a state = Start | Cur of 'a | Done

fun concatMap f gen =
   let
      val state = ref Start
      fun gen' () =
         case !state of
            Start => next ()
          | Done => raise Stop
          | Cur g => g () handle Stop => next ()

      and next () =
         (state := Cur (f (gen ())); gen' ())
         handle e => (state := Done; raise e)

   in
      gen'
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
