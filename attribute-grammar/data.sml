
structure Str:
sig
  datatype 'a t = Zero | One of 'a | Two of 'a t * 'a t

  val same: ('a * 'a -> bool) -> 'a t * 'a t -> bool
  val toString: ('a -> string) -> 'a t -> string
  val map: ('a -> 'b) -> 'a t -> 'b t
  val toList: 'a t -> 'a list
  val fromList: 'a list -> 'a t
  val toAndFrom: 'a t -> 'a list * ('a list -> 'a t)
end =
struct
  datatype 'a t = Zero | One of 'a | Two of 'a t * 'a t

  fun same _ (Zero, Zero) = true
    | same eq (One a, One b) = eq (a, b)
    | same eq (Two (a, b), Two (c, d)) = same eq (a, c) andalso same eq (b, d)
    | same _ _ = false

  fun toString _ Zero = "Zero"
    | toString s (One a) = "One " ^ s a
    | toString s (Two (a, b)) = "Two (" ^ toString s a ^ ", " ^ toString s b ^ ")"

  fun map _ Zero = Zero
    | map f (One a) = One (f a)
    | map f (Two (a, b)) = Two (map f a, map f b)

  fun toList Zero = []
    | toList (One a) = [a]
    | toList (Two (a, b)) = toList a @ toList b

  fun fromList [] = Zero
    | fromList (x::xs) = Two (One x, fromList xs)

  fun toAndFrom x = 
    let
      fun g (Zero, acc) = acc
        | g (One x, acc) = x :: acc
        | g (Two (a, b), acc) = g (a, g (b, acc))

      fun f Zero rs = (Zero, rs)
        | f (One _) (r::rs) = (One r, rs)
        | f (One _) [] = raise Fail "Invalid transformation"
        | f (Two (a, b)) rs =
            let
              val (a', rs) = f a rs
              val (b', rs) = f b rs
            in
              (Two (a', b'), rs)
            end
    in
      (g (x, []), #1 o f x)
    end
end

structure Direct =
struct
  infix |* |+ |- ||* ||+

  type ('from, 'to) typ = 'to Str.t * ('to Str.t -> 'from)
  exception Invalid

  fun plate f = (Str.Zero, fn _ => f)
  val _: 'from -> ('from, 'to) typ = plate

  fun plateStar f x = (Str.One x, fn Str.One x => f x | _ => raise Invalid)
  val _: ('to -> 'from) -> 'to -> ('from, 'to) typ = plateStar

  fun platePlus biplate f x =
    case biplate x of
        (ys, y) => (ys, fn ys => f (y ys))
  val _: ('item -> ('item, 'to) typ) -> ('item -> 'from) -> 'item -> ('from, 'to) typ = platePlus

  fun (xs, x) |* y =
    (Str.Two (xs, Str.One y), fn Str.Two (xs, Str.One y) => x (xs, y) | _ => raise Invalid)
end
