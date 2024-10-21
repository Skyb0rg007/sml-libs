
structure Property =
struct

datatype t = T of int * exn list ref

val counter = ref 0

fun new () = T (!counter, ref []) before counter := !counter + 1

end

signature PROPERTY =
sig

type t

val get: Property.t -> t option
val put: Property.t * t -> unit

end

functor PLProperty(type t) :> PROPERTY where type t = t =
struct

type t = t

exception E of t

fun get (Property.T (_, xs)) =
   let
      fun go (E x :: _) = SOME x
        | go (_ :: xs) = go xs
        | go [] = NONE
   in
      go (!xs)
   end

fun put (Property.T (_, xs), x) =
   let
      fun go (E _ :: xs) = E x :: xs
        | go (y :: xs) = y :: go xs
        | go [] = [E x]
   in
      xs := go (!xs)
   end

end

functor HTProperty(type t) :> PROPERTY where type t = t =
struct

type t = t

val cache: t IntHashTable.hash_table =
   IntHashTable.mkTable (32, Fail "HTProperty.cache failed lookup")

fun get (Property.T (n, _)) = IntHashTable.find cache n

fun put (Property.T (n, _), x) = IntHashTable.insert cache (n, x)

end

structure Property :>
   sig
      type t = Property.t

      val new: unit -> t
   end = Property

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
