
signature PROFUNCTOR =
  sig
    type ('a, 'b) t

    val lmap : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
    val rmap : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  end

signature COREPRESENTABLE =
  sig
    include PROFUNCTOR

    (* Associated Functor *)
    structure F :
      sig
        type 'a t

        val map : ('a -> 'b) -> 'a t -> 'b t
      end

    (* ('a, 'b) t ≅ 'a F.t -> 'b *)
    val cosieve : ('a, 'b) t -> 'a F.t -> 'b
    val cotabulate : ('a F.t -> 'b) -> ('a, 'b) t
  end
