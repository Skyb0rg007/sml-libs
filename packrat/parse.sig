
signature PARSE =
sig
   type 'a t

   val terminal: string -> string t
   val choice: 'a t * 'b t -> ('a, 'b) Either.either t
   val sequence: 'a t * 'b t -> ('a * 'b) t
   val not: 'a t -> unit t
   val many: 'a t -> 'a list t
end

(* vim: set tw=0 ts=3 sw=3: *)
