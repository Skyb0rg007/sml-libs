
signature PARSER_STRUCTS =
   sig
      structure Kind:
         sig
            type t

            val compare: t * t -> order
         end

      structure Token:
         sig
            type t

            val kind: t -> Kind.t
         end

      structure Nonterm:
         sig
            type t

            val + : t * t -> t
            val token: Token.t -> t
         end
   end

signature PARSER =
   sig
      type kind
      type token
      type nonterm

      type mark = string

      type syntax

      val epsilon: nonterm -> syntax
      val elem: kind -> syntax
      val map: (nonterm -> nonterm) -> syntax -> syntax
      val * : syntax * syntax -> syntax
      val + : syntax * syntax -> syntax
      val fix: (syntax -> syntax) -> syntax

      val parse: syntax * token list -> nonterm option
   end

(* vim: set tw=0 sw=3 ts=3: *)
