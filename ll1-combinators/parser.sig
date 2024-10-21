
signature PARSER_STRUCTS =
   sig
      structure Kind:
         sig
            type t

            val toInt: t -> int
         end

      structure Token:
         sig
            type t

            val kind: t -> Kind.t
         end
   end

signature PARSER =
   sig
      type kind
      type token
      type 'a t

      val pure: 'a -> 'a t
      val map: ('a -> 'b) -> 'a t -> 'b t
      val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
      val map3: ('a * 'b * 'c -> 'd) -> 'a t * 'b t * 'c t -> 'd t
      val empty: 'a t
      val choice: 'a t * 'a t -> 'a t
      val fix: ('a t -> 'a t) -> 'a t
      val kind: kind -> token t

      val parse: 'a t * token list -> 'a option
      (* val parse: (token, 's) StringCvt.reader -> ('a, 's) StringCvt.reader *)
   end

(* vim: set tw=0 ts=3 sw=3: *)
