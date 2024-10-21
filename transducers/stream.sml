
signature STREAM =
sig
  type ('a, 'b, 'acc) sink

  val map : ('b -> 'c) -> ('a, 'b, 'acc) sink -> ('a, 'c, 'acc) sink
  val premap : ('c -> 'a) -> ('a, 'b, 'acc) sink -> ('c, 'b, 'acc) sink
  val prefilter : ('a -> bool) -> ('a, 'b, 'acc) sink -> ('a, 'b, 'acc) sink
  val zipWith : ('b * 'c -> 'd) -> ('a, 'b, 'acc1) sink * ('a, 'c, 'acc2) sink -> ('a, 'd, 'acc1 * 'acc2) sink
  val unzipWith : ('b * 'd -> 'e) -> ('a, 'b, 'acc1) sink * ('c, 'd, 'acc2) sink -> ('a * 'c, 'e, 'acc1 * 'acc2) sink
end

structure Stream : STREAM =
struct
  datatype ('a, 'b, 'acc) sink = Sink of {
    init : unit -> 'acc,
    push : 'a * 'acc -> 'acc,
    full : 'acc -> bool,
    stop : 'acc -> 'b
  }

  fun map f (Sink {init, push, full, stop}) =
    Sink {
      init = init,
      push = push,
      full = full,
      stop = fn x => f (stop x)
    }

  fun premap f (Sink {init, push, full, stop}) =
    Sink {
      init = init,
      push = fn (a, acc) => push (f a, acc),
      full = full,
      stop = stop
    }

  fun prefilter f (Sink {init, push, full, stop}) =
    Sink {
      init = init,
      push = fn (a, acc) => if f a then push (a, acc) else acc,
      full = full,
      stop = stop
    }

  fun zipWith f (Sink s1, Sink s2) =
    Sink {
      init = fn () => (#init s1 (), #init s2 ()),
      push = fn (a, (acc1, acc2)) => (#push s1 (a, acc1), #push s2 (a, acc2)),
      full = fn (acc1, acc2) => #full s1 acc1 orelse #full s2 acc2,
      stop = fn (acc1, acc2) => f (#stop s1 acc1, #stop s2 acc2)
    }

  fun unzipWith f (Sink s1, Sink s2) =
    Sink {
      init = fn () => (#init s1 (), #init s2 ()),
      push = fn ((a, b), (acc1, acc2)) => (#push s1 (a, acc1), #push s2 (b, acc2)),
      full = fn (acc1, acc2) => #full s1 acc1 orelse #full s2 acc2,
      stop = fn (acc1, acc2) => f (#stop s1 acc1, #stop s2 acc2)
    }

  (* fun concatMap f (Sink {init, push, full, stop}) = *)
  (*   Sink { *)
  (*     init = fn () => Either.INL (init ()), *)
  (*     push = fn (a, Either.INL acc) => push (a, acc), *)
  (*     full = fn acc => full acc, *)
  (*     stop = fn acc => f (stop acc) *)
  (*   } *)


end
