
structure Cont = SMLofNJ.Cont

structure Transducer = struct

  type 'a cont = 'a Cont.cont

  datatype ('a, 'b) channel = Channel of ('a * ('b, 'a) channel) cont

  type 'a upchan = (unit, 'a) channel
  type 'a downchan = ('a, unit) channel
  type ('a, 'r) source = 'a downchan -> 'r
  type ('a, 'r) sink = 'a downchan -> 'r

  type ('a, 'b, 'r) transducer = 'a upchan * 'a downchan -> 'r

  fun switch (x, Channel k) =
    Cont.callcc (fn k' => Cont.throw k (x, Channel k'))

  infix >>

  fun (t1 >> t2) (u, d) = Cont.callcc (fn k =>
    t1 (u, #2 (Cont.callcc (fn k' =>
      Cont.throw k (t2 (Channel k', d))))))

end

structure Hyper : sig

  type ('a, 'b) t

  val invoke : ('a, 'b) t * ('b, 'a) t -> 'b
  val unroll : ('a, 'b) t * (('a, 'b) t -> 'a) -> 'b
  val roll : ((('a, 'b) t -> 'a) -> 'b) -> ('a, 'b) t
  val ana : ('x * ('x -> 'a) -> 'b) -> 'x -> ('a, 'b) t
  val cata : ((('x -> 'a) -> 'b) -> 'x) -> ('a, 'b) t -> 'x
  val arr : ('a -> 'b) -> ('a, 'b) t
  val id : ('a, 'a) t
  val run : ('a, 'a) t -> 'a

end = struct

  datatype ('a, 'b) t = Hyper of ('b, 'a) t -> 'b

  fun invoke (Hyper k, f) = k f

  fun unroll (h, f) = invoke (h, Hyper f)

  fun roll h = Hyper (fn k => invoke (Hyper h, k))

  fun ana psi = let
    fun f x = Hyper (fn z => psi (x, fn q => invoke (z, f q)))
    in f end

  fun cata phi = let
    fun f h = phi (fn g => unroll (h, g o f))
    in f end

  fun arr f = let
    fun h () = Hyper (fn k => f (invoke (k, h ())))
    in h () end

  local
    fun h () = Hyper (fn k => invoke (k, h ()))
  in
    val id = Hyper (fn k => invoke (k, h ()))
  end

  fun run h = invoke (h, id)

  fun pure x = Hyper (fn _ => x)

  fun ap (f, g) = ana (fn ((i, j), fga) =>
    unroll (i, fn i' => fga (i', j))
      (unroll (j, fn j' => fga (i, j'))))

  fun m >>= f = cata (fn g => roll (fn k => unroll (f (g k), k), m))
end
