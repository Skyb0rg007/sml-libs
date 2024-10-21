
structure BankersDeque =
struct

structure Stream =
  struct
    datatype 'a node = Nil | Cons of 'a * 'a t
    withtype 'a t = unit -> 'a node

    fun take n s () =
      if n <= 0
        then Nil
      else case s () of
              Nil => Nil
            | Cons (x, xs) => Cons (x, take (n - 1) xs)

      fun drop n s () =
        if n <= 0
          then s ()
        else case s () of
                Nil => Nil
              | Cons (_, xs) => drop (n - 1) xs ()

    fun append s1 s2 () =
      case s1 () of
          Nil => s2 ()
        | Cons (x, xs) => Cons (x, append xs s2)
  end

datatype 'a t = Queue of {
    front : 'a Stream.t,
    lenF : int,
    rear : 'a Stream.t,
    lenR : int
  }

exception Empty

(* fun queue (f, lf, r, lr) = *)
(*   if lf > c * lr + 1 *)
(*     then *)
(*       let *)
(*         val i = (lf + lr) div 2 *)
(*         val j = lf + lr - i *)
(*         val f' = Stream.take i f *)
(*         val r' = Stream.append *) 
(*       in *)
(*       end *)

(* val empty = Queue { *)
(*     front = fn _ => Nil, *)
(*     lenF = 0, *)
(*     rear = fn _ => Nil, *)
(*     lenR = 0 *)
(*   } *)

(* fun cons (x, Queue {front, lenF, rear, lenR}) = *)
(*   queue (fn _ => Cons (x, front), lenF + 1, rear, lenR) *)

end
