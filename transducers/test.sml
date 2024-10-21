
structure Test =
struct
  structure R = Reducer

  fun run () =
    let
      val xs = [1, 2, 3, 4, 5, 6, 7, 8, 9]
      fun dup x = (x, fn () => x)
      val f : (int * (unit -> int), int * (unit -> int)) R.t = R.dimap #2 dup R.length
      val fold : (int, int) R.t = R.unfirst f
      val ys = R.cosieve fold xs

      (* val ys = List.map Int.toString ys *)
    in
      print ("ys = " ^ Int.toString ys ^ "\n")
      (* print ("ys = [" ^ String.concatWith "," ys ^ "]\n") *)
    end
end
