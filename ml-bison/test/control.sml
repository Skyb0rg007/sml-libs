
signature GREAT_ESCAPE =
  sig
    type ans

    val reset : (unit -> ans) -> ans
    val shift : (('a -> ans) -> ans) -> 'a
  end

functor GreatEscape(type ans) :> GREAT_ESCAPE where type ans = ans =
  struct
    structure Cont = SMLofNJ.Cont

    exception MissingReset

    datatype result = Ok of ans | Exn of exn

    type ans = ans

    val mk =
      ref (Cont.isolate (fn _ : unit -> result => raise MissingReset))

    fun abort x = Cont.throw (!mk) x

    fun reset t =
      let
        fun f k =
          let
            val m = !mk
            val () = mk := (Cont.isolate (fn x => (mk := m; Cont.throw k x)))
            val x = Ok (t ()) handle e => Exn e
          in
            abort (fn () => x)
          end
      in
        case Cont.callcc f () of
            Ok x => x
          | Exn e => raise e
      end

    fun shift h =
      Cont.callcc
        (fn k =>
          abort (fn () =>
            Ok (reset (fn () => h (fn v => reset (fn () => Cont.throw k v))))))
  end
