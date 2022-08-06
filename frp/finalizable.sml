
signature FINALIZABLE =
sig
   type 'a t

   val addFinalizer: 'a t * ('a -> unit) -> unit
   val finalizeBefore: 'a t * 'b t -> unit
   val new: 'a -> 'a t
   val touch: 'a t -> unit
   val withValue: 'a t * ('a -> 'b) -> 'b

   (* SML/NJ doesn't have GC hooks, so this must be called manually *)
   val doGC: unit -> unit
end

structure Finalizable :> FINALIZABLE =
struct
   structure Weak = SMLofNJ.Weak
   structure CleanUp = SMLofNJ.Internals.CleanUp
   structure GC = SMLofNJ.Internals.GC

   datatype 'a t = T of {
      afters: (unit -> unit) list ref,
      finalizers: ('a -> unit) list ref,
      value: 'a ref
   }

   (* TODO: Ask Reppy how to do this *)
   fun touch (T {value, ...}) = ignore (Fn.id value)

   fun withValue (f as T {value, ...}, g) =
      case Either.INL (g (!value)) handle e => Either.INR e of
         Either.INL a => (touch f; a)
       | Either.INR e => (touch f; raise e)

   fun addFinalizer (T {finalizers, ...}, f) =
      finalizers := f :: !finalizers

   fun finalizeBefore (T {afters, ...}, f) =
      afters := (fn () => touch f) :: !afters

   local
      type info = {clean: unit -> unit, isAlive: unit -> bool}
      val r: info list ref = ref []

      fun clean l =
         List.foldl
            (fn (z as {clean, isAlive}, (gotOne, zs)) =>
               if isAlive ()
                  then (gotOne, z :: zs)
               else (clean (); (true, zs)))
            (false, []) l

      val _ = CleanUp.addCleaner
         ("Finalizable structure at-exit cleaner",
          [CleanUp.AtExit],
          (fn _ =>
            let
               val l = !r
               val () = r := []
               fun loop l =
                  let
                     val () = GC.doGC 6
                     val (gotOne, l) = clean l
                  in
                     if gotOne then loop l else ()
                  end
            in
               loop l
            end))
   in
      fun finalize z = r := z :: !r

      fun doGC () = r := #2 (clean (!r))
   end

   fun new x =
      let
         val afters = ref []
         val finalizers = ref []
         val value = ref x
         val f = T {afters = afters, finalizers = finalizers, value = value}
         val weak = Weak.weak value
         fun clean () =
            (List.app (fn f => f x) (!finalizers)
             ; List.app (fn f => f ()) (!afters))
         fun isAlive () = Option.isSome (Weak.strong weak)
      in
         finalize {clean = clean, isAlive = isAlive}
         ; f
      end
end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
