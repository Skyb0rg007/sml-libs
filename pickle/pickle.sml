
signature PICKLE =
sig
   type 'a t

   val null : unit t
   val empty : unit t
   val ignore : unit t
   val constant : string -> unit t
   val bool : bool t
   val int : int t
   val intInf : IntInf.int t
   val array : 'a t -> 'a array t
   val list : 'a t -> 'a list t
   val mu : ('a t -> 'a t) -> 'a t
end

(* vim: set tw=0 ts=3 sw=3: *)
