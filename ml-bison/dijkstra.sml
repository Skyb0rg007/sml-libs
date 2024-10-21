
structure Dial =
struct
  structure DynArray :
    sig
      type 'a t

      val array : int * 'a -> 'a t
      val sub : 'a t * int -> 'a
      val update : 'a t * int * 'a -> unit
      val length : 'a t -> int
    end =
    struct
      datatype 'a t = DynArray of 'a Array.array ref * int ref * 'a

      fun array (n, x) = DynArray (ref (Array.array (n, x)), ref 0, x)

      fun length (DynArray (_, n, _)) = !n

      fun reserve (d as DynArray (arr, _, def), n) =
        let
          val cap = Array.length (!arr)
        in
          if n <= cap
            then ()
          else
            let
              val newArr = Array.array (n, def)
            in
              Array.copy { dst = newArr, src = !arr, di = 0 };
              arr := newArr
            end
        end

      fun sub (DynArray (arr, sz, def), i) =
        if i < !sz
          then Array.sub (!arr, i)
        else def

      fun update (d as DynArray (arr, sz, _), i, x) =
        (reserve (d, i + 1);
         Array.update (!arr, i, x);
         sz := Int.max (!sz, i + 1))
    end

  fun shortestPaths start neighbors success =
    let
      val todo = DynArray.array (8, [])
      val () = DynArray.update (todo, 0, [start])

      fun findFirst c =
        if c > DynArray.length todo
          then []
        else
          case DynArray.sub (todo, c) of
              [] => findFirst (c + 1)
            | n :: ns => (
                DynArray.update (todo, c, ns);
                if success n
                  then findRest (c, [n], DynArray.sub (todo, c))
                else (
                  List.app
                    (fn (c', v) =>
                      DynArray.update (todo, c', v :: DynArray.sub (todo, c')))
                    (neighbors {all = true} n);
                  findFirst c))

      and findRest (_, results, []) = results
        | findRest (c, results, check :: rest) =
          if success check
            then findRest (c, check :: results, rest)
          else
            let
              val rest' =
                List.foldl
                  (fn ((c', v), rest) =>
                    if c' = c
                      then v :: rest
                    else rest)
                  rest
                  (neighbors {all = false} check)
            in
              findRest (c, results, rest')
            end
    in
      findFirst 0
    end

  val _ : 's -> ({all: bool} -> 's -> (int * 's) list) -> ('s -> bool) -> 's list = shortestPaths

  (* `start : 's` The starting state
   * `neighbors : {all:bool} -> 's -> (int * 's) list`
   *    Calculate the neighbors of a given state along with the costs of those edges
   *    If `all` is true, this must return all neighbors
   *    If `all` is false, this function should only return neighbors that are
   *    distance 0 from the node
   * `success : 's -> bool` : Is the state a success?
   *
   * Returns all states which are successes, which also have minimal cost.
   *)
end

