
functor ParserFn(S: PARSER_STRUCTS): PARSER =
struct

open S

structure IntSet = IntRedBlackSet
structure KindSet = RedBlackSetFn(
   struct
      type ord_key = Kind.t
      val compare = Kind.compare
   end)

type kind = Kind.t
type nonterm = Nonterm.t
type token = Token.t
type mark = string

(* Cell containing a boolean flag. Can be set (but not unset!) *)
structure BoolCell =
   struct
      datatype t = Cell of (unit -> unit) list ref * bool ref

      fun new b = Cell (ref [], ref b)

      fun get (Cell (_, b)) = !b

      fun subscribe (Cell (rs, b), k) =
         if !b
            then k ()
            else rs := k :: !rs

      fun set (Cell (rs, b)) =
         if !b
            then ()
            else (b := true; List.app (fn k => k ()) (!rs); rs := [])
   end

(* Cell containing an increasing set of kinds *)
structure KindsCell =
   struct
      datatype t = Cell of (KindSet.set -> unit) list ref * KindSet.set ref

      fun new s = Cell (ref [], ref s)

      fun get (Cell (_, s)) = !s

      fun subscribe (Cell (rs, s), k) =
         (rs := k :: !rs;
          if KindSet.isEmpty (!s) then () else k (!s))

      fun add (Cell (rs, s), set) =
         let
            val diff = KindSet.difference (set, !s)
         in
            if KindSet.isEmpty diff
               then ()
               else (s := KindSet.union (!s, diff);
                     List.app (fn k => k diff) (!rs))
         end
   end

structure IdsCell =
   struct
      datatype t = Cell of (IntSet.set -> unit) list ref * IntSet.set ref

      fun new s = Cell (ref [], ref s)

      fun get (Cell (_, s)) = !s

      fun subscribe (Cell (rs, s), k) =
         (rs := k :: !rs;
          if IntSet.isEmpty (!s) then () else k (!s))

      fun add (Cell (rs, s), set) =
         let
            val diff = IntSet.difference (set, !s)
         in
            if IntSet.isEmpty diff
               then ()
               else (s := IntSet.union (!s, diff);
                     List.app (fn k => k diff) (!rs))
         end
   end

(* Stores nothing, or one value *)
structure OptionCell =
   struct
      datatype 'a t = Cell of ('a -> unit) list ref * 'a option ref

      fun new opt = Cell (ref [], ref opt)

      fun get (Cell (_, opt)) = !opt

      fun subscribe (Cell (rs, opt), k) =
         case !opt of
            SOME x => k x
          | NONE => rs := k :: !rs

      fun set (Cell (rs, opt), x) =
         case !opt of
            SOME _ => ()
          | NONE => (opt := SOME x;
                     List.app (fn k => k x) (!rs);
                     rs := [])
   end

(* Doesn't propagate value until activated *)
structure GateCell =
   struct
      datatype t = Cell of (KindSet.set -> unit) list ref * KindSet.set ref * bool ref

      fun new () = Cell (ref [], ref KindSet.empty, ref false)

      fun get (Cell (_, set, _)) = !set

      fun subscribe (Cell (rs, set, active), k) =
         (rs := k :: !rs;
          if !active then k (!set) else ())

      fun activate (Cell (rs, set, active)) =
         if !active
            then ()
            else (active := true;
                  List.app (fn k => k (!set)) (!rs))

      fun add (Cell (rs, set, active), s) =
         (set := KindSet.union (!set, s);
          if !active then List.app (fn k => k s) (!rs) else ())
   end

structure IdGateCell =
   struct
      datatype t = Cell of (IntSet.set -> unit) list ref * IntSet.set ref * bool ref

      fun new () = Cell (ref [], ref IntSet.empty, ref false)

      fun get (Cell (_, set, _)) = !set

      fun subscribe (Cell (rs, set, active), k) =
         (rs := k :: !rs;
          if !active then k (!set) else ())

      fun activate (Cell (rs, set, active)) =
         if !active
            then ()
            else (active := true;
                  List.app (fn k => k (!set)) (!rs))

      fun add (Cell (rs, set, active), s) =
         (set := IntSet.union (!set, s);
          if !active then List.app (fn k => k s) (!rs) else ())
   end

datatype syntax = Syntax of {
      productive: BoolCell.t,
      nullable: nonterm OptionCell.t,
      first: KindsCell.t,
      shouldNotFollow: KindsCell.t,
      visitable: IdsCell.t,
      node: node
   }

and node =
   Success of nonterm
 | Failure
 | Elem of kind
 | Marked of syntax * mark
 | Transform of syntax * (nonterm -> nonterm)
 | Sequence of syntax * syntax
 | Disjunction of syntax * syntax
 | Recursive of int * (unit -> syntax)

local
   fun go acc (Syntax {node, ...}) =
      case node of
         Recursive (n, s) =>
            if IntSet.member (acc, n)
               then acc
               else go (IntSet.add (acc, n)) (s ())
       | Disjunction (a, b) => go (go acc a) b
       | Sequence (a, b) =>
            let
               val Syntax {nullable, ...} = a
            in
               if Option.isSome (OptionCell.get nullable)
                  then go (go acc a) b
                  else go acc a
            end
       | Transform (s, _) => go acc s
       | Marked (s, _) => go acc s
       | Success _ => acc
       | Failure => acc
       | Elem _ => acc
in
   fun visitable s = go IntSet.empty s
end

local
   (* Reuse the cells in leaf nodes *)
   val falseCell = BoolCell.new false
   val trueCell = BoolCell.new true
   val emptyCell = KindsCell.new KindSet.empty
   val emptyCell' = IdsCell.new IntSet.empty
in
   fun epsilon x =
      Syntax {
         productive = trueCell,
         nullable = OptionCell.new (SOME x),
         first = emptyCell,
         shouldNotFollow = emptyCell,
         visitable = emptyCell',
         node = Success x
      }

   fun failure () =
      Syntax {
         productive = falseCell,
         nullable = OptionCell.new NONE,
         first = emptyCell,
         shouldNotFollow = emptyCell,
         visitable = emptyCell',
         node = Failure
      }

   fun elem kind =
      Syntax {
         productive = trueCell,
         nullable = OptionCell.new NONE,
         first = KindsCell.new (KindSet.singleton kind),
         shouldNotFollow = emptyCell,
         visitable = emptyCell',
         node = Elem kind
      }
end

fun map f (s as Syntax {productive, nullable, first, shouldNotFollow, visitable, node}) =
   case node of
      Failure => failure ()
    | Transform (s, g) =>
         Syntax {
            productive = productive,
            nullable = nullable,
            first = first,
            shouldNotFollow = shouldNotFollow,
            visitable = visitable,
            node = Transform (s, g o f)
         }
    | _ =>
         Syntax {
            productive = productive,
            nullable = nullable,
            first = first,
            shouldNotFollow = shouldNotFollow,
            visitable = visitable,
            node = Transform (s, f)
         }

fun left + right =
   let
      val Syntax {productive = lProductive, ...} = left
      val Syntax {productive = rProductive, ...} = right
      val Syntax {nullable = lNullable, ...} = left
      val Syntax {nullable = rNullable, ...} = right
      val Syntax {first = lFirst, ...} = left
      val Syntax {first = rFirst, ...} = right
      val Syntax {shouldNotFollow = lSnf, ...} = left
      val Syntax {shouldNotFollow = rSnf, ...} = right
      val Syntax {visitable = lVisitable, ...} = left
      val Syntax {visitable = rVisitable, ...} = right

      fun assertLookahead (lFst, rFst) =
         if KindSet.isEmpty (KindSet.intersection (lFst, rFst))
            then ()
            else raise Fail "Disjunction requires >1 tokens of lookahead"

      fun assertNonnull NONE = ()
        | assertNonnull (SOME _) = raise Fail "Disjunction of two nullable parsers"

      val productive = BoolCell.new false
      val nullable = OptionCell.new NONE
      val first = KindsCell.new KindSet.empty
      val snf = KindsCell.new KindSet.empty
      val visitable = IdsCell.new IntSet.empty

      val snfLeft = GateCell.new ()
      val snfRight = GateCell.new ()
   in
      (* Error checking *)
      KindsCell.subscribe (lFirst, fn x => assertLookahead (x, KindsCell.get rFirst));
      KindsCell.subscribe (rFirst, fn y => assertLookahead (KindsCell.get lFirst, y));
      OptionCell.subscribe (lNullable, fn _ => assertNonnull (OptionCell.get rNullable));
      OptionCell.subscribe (rNullable, fn _ => assertNonnull (OptionCell.get lNullable));

      (* PRODUCTIVE(s₁) ==> PRODUCTIVE(s₁ ∨ s₂) *)
      BoolCell.subscribe (lProductive, fn () => BoolCell.set productive);
      (* PRODUCTIVE(s₂) ==> PRODUCTIVE(s₁ ∨ s₂) *)
      BoolCell.subscribe (rProductive, fn () => BoolCell.set productive);

      (* NULLABLE(s₁, v) ==> NULLABLE(s₁ ∨ s₂, v) *)
      OptionCell.subscribe (lNullable, fn v => OptionCell.set (nullable, v));
      (* NULLABLE(s₂, v) ==> NULLABLE(s₁ ∨ s₂, v) *)
      OptionCell.subscribe (rNullable, fn v => OptionCell.set (nullable, v));

      (* k ∈ FIRST(s₁) ==> k ∈ FIRST(s₁ ∨ s₂) *)
      KindsCell.subscribe (lFirst, fn s => KindsCell.add (first, s));
      (* k ∈ FIRST(s₂) ==> k ∈ FIRST(s₁ ∨ s₂) *)
      KindsCell.subscribe (rFirst, fn s => KindsCell.add (first, s));

      (* k ∈ SN-FOLLOW(s₁) ==> k ∈ SN-FOLLOW(s₁ ∨ s₂) *)
      KindsCell.subscribe (lSnf, fn s => KindsCell.add (snf, s));
      (* k ∈ SN-FOLLOW(s₂) ==> k ∈ SN-FOLLOW(s₁ ∨ s₂) *)
      KindsCell.subscribe (rSnf, fn s => KindsCell.add (snf, s));
      (* k ∈ FIRST(s₁) ∧ NULLABLE(s₂, v) ==> k ∈ SN-FOLLOW(s₁ ∨ s₂) *)
      KindsCell.subscribe (lFirst, fn s => GateCell.add (snfLeft, s));
      OptionCell.subscribe (rNullable, fn _ => GateCell.activate snfLeft);
      GateCell.subscribe (snfLeft, fn s => KindsCell.add (snf, s));
      (* NULLABLE(s₁, v) ∧ k ∈ FIRST(s₂) ==> k ∈ SN-FOLLOW(s₁ ∨ s₂) *)
      KindsCell.subscribe (rFirst, fn s => GateCell.add (snfRight, s));
      OptionCell.subscribe (lNullable, fn _ => GateCell.activate snfRight);
      GateCell.subscribe (snfRight, fn s => KindsCell.add (snf, s));

      (* x ∈ VISITABLE(s₁) ==> x ∈ VISITABLE(s₁ ∨ s₂) *)
      IdsCell.subscribe (lVisitable, fn s => IdsCell.add (visitable, s));
      (* x ∈ VISITABLE(s₂) ==> x ∈ VISITABLE(s₁ ∨ s₂) *)
      IdsCell.subscribe (rVisitable, fn s => IdsCell.add (visitable, s));

      Syntax {
         productive = productive,
         nullable = nullable,
         first = first,
         shouldNotFollow = snf,
         visitable = visitable,
         node = Disjunction (left, right)
      }
   end

fun left * right =
   let
      val Syntax {productive = lProductive, ...} = left
      val Syntax {productive = rProductive, ...} = right
      val Syntax {nullable = lNullable, ...} = left
      val Syntax {nullable = rNullable, ...} = right
      val Syntax {first = lFirst, ...} = left
      val Syntax {first = rFirst, ...} = right
      val Syntax {shouldNotFollow = lSnf, ...} = left
      val Syntax {shouldNotFollow = rSnf, ...} = right
      val Syntax {visitable = lVisitable, ...} = left
      val Syntax {visitable = rVisitable, ...} = right

      val productive = BoolCell.new false
      val nullable = OptionCell.new NONE
      val first = KindsCell.new KindSet.empty
      val snf = KindsCell.new KindSet.empty
      val visitable = IdsCell.new IntSet.empty

      val firstLeft = GateCell.new ()
      val firstRight = GateCell.new ()
      val snfLeft = GateCell.new ()
      val snfRight = GateCell.new ()
      val visitableGate = IdGateCell.new ()
   in
      (* PRODUCTIVE(s₁) ∧ PRODUCTIVE(s₂) ==> PRODUCTIVE(s₁ ⋅ s₂) *)
      BoolCell.subscribe (lProductive, fn () => if BoolCell.get rProductive then BoolCell.set productive else ());
      BoolCell.subscribe (rProductive, fn () => if BoolCell.get lProductive then BoolCell.set productive else ());

      (* NULLABLE(s₁, v₁) ∧ NULLABLE(s₂, v₂) ==> NULLABLE(s₁ ⋅ s₂, (v₁, v₂)) *)
      OptionCell.subscribe (lNullable, fn x => case OptionCell.get rNullable of NONE => () | SOME y => OptionCell.set (nullable, Nonterm.+ (x, y)));
      OptionCell.subscribe (rNullable, fn y => case OptionCell.get lNullable of NONE => () | SOME x => OptionCell.set (nullable, Nonterm.+ (x, y)));

      (* k ∈ FIRST(s₁) ∧ PRODUCTIVE(s₂) ==> k ∈ FIRST(s₁ ⋅ s₂) *)
      KindsCell.subscribe (lFirst, fn s => GateCell.add (firstLeft, s));
      BoolCell.subscribe (rProductive, fn () => GateCell.activate firstLeft);
      GateCell.subscribe (firstLeft, fn s => KindsCell.add (first, s));
      (* NULLABLE(s₁, v) ∧ k ∈ FIRST(s₂) ==> k ∈ FIRST(s₁ ⋅ s₂) *)
      KindsCell.subscribe (rFirst, fn s => GateCell.add (firstRight, s));
      OptionCell.subscribe (lNullable, fn _ => GateCell.activate firstRight);
      GateCell.subscribe (firstRight, fn s => KindsCell.add (first, s));

      (* k ∈ SN-FOLLOW(s₁) ∧ NULLABLE(s₂, v) ==> k ∈ SN-FOLLOW(s₁ ⋅ s₂) *)
      KindsCell.subscribe (lSnf, fn s => GateCell.add (snfLeft, s));
      OptionCell.subscribe (rNullable, fn _ => GateCell.activate snfLeft);
      GateCell.subscribe (snfLeft, fn s => KindsCell.add (snf, s));
      (* PRODUCTIVE(s₁) ∧ k ∈ SN-FOLLOW(s₂) ==> k ∈ FIRST(s₁ ⋅ s₂) *)
      KindsCell.subscribe (rSnf, fn s => GateCell.add (snfRight, s));
      BoolCell.subscribe (lProductive, fn () => GateCell.activate snfRight);
      GateCell.subscribe (snfRight, fn s => KindsCell.add (snf, s));

      (* NULLABLE(s₁, v) ∧ x ∈ VISITABLE(s₂) ==> x ∈ VISITABLE(s₁ ⋅ s₂) *)
      OptionCell.subscribe (lNullable, fn _ => IdGateCell.activate visitableGate);
      IdsCell.subscribe (rVisitable, fn s => IdGateCell.add (visitableGate, s));
      IdGateCell.subscribe (visitableGate, fn s => IdsCell.add (visitable, s));

      Syntax {
         productive = productive,
         nullable = nullable,
         first = first,
         shouldNotFollow = snf,
         visitable = visitable,
         node = Sequence (left, right)
      }
   end

local
   val counter = ref 0
in
   fun fix f =
      let
         val n = !counter before counter := Int.+ (!counter, 1)
         val productive = BoolCell.new false
         val nullable = OptionCell.new NONE
         val first = KindsCell.new KindSet.empty
         val snf = KindsCell.new KindSet.empty
         val visitable = IdsCell.new IntSet.empty

         val selfRef = ref NONE
         val self = Syntax {
               productive = productive,
               nullable = nullable,
               first = first,
               shouldNotFollow = snf,
               visitable = visitable,
               node = Recursive (n, fn () => valOf (!selfRef))
            }
         val result = f self
         val () = selfRef := SOME result
         val Syntax {productive = rProd, nullable = rNull, first = rFirst, shouldNotFollow = rSnf, visitable = rVisitable, ...} = result
      in
         BoolCell.subscribe (rProd, fn () => BoolCell.set productive);
         OptionCell.subscribe (rNull, fn x => OptionCell.set (nullable, x));
         KindsCell.subscribe (rFirst, fn s => KindsCell.add (first, s));
         KindsCell.subscribe (rSnf, fn s => KindsCell.add (snf, s));
         IdsCell.subscribe (rVisitable, fn s => IdsCell.add (visitable, s));
         IdsCell.subscribe (visitable, fn s =>
            if IntSet.member (s, n)
               then raise Fail "Left-recursion"
               else ());

         self
      end
end

local
   datatype layer =
      Apply of nonterm -> nonterm
    | Prepend of nonterm
    | FollowBy of syntax

   type focus = syntax * layer list

   fun focus s = (s, [])

   fun plug (v, []) = (epsilon v, [])
     | plug (v, Apply f :: c) = plug (f v, c)
     | plug (v, Prepend v' :: c) = plug (Nonterm.+ (v', v), c)
     | plug (v, FollowBy s :: c) = (s, Prepend v :: c)

   fun nullable (s as Syntax {nullable = n, ...}, c) =
      case OptionCell.get n of
         NONE => NONE
       | SOME v => if List.null c then SOME v else nullable (plug (v, c))

   fun locate (k, (s as Syntax {first, nullable, ...}, c)) =
      if KindSet.member (KindsCell.get first, k)
         then SOME (s, c)
         else
            case OptionCell.get nullable of
               NONE => NONE
             | SOME v => if List.null c then NONE else locate (k, plug (v, c))

   fun pierce (k, (s as Syntax {node, ...}, c)) =
      case node of
         Elem _ => c
       | Disjunction (s1 as Syntax {first, ...}, s2) =>
            if KindSet.member (KindsCell.get first, k)
               then pierce (k, (s1, c))
               else pierce (k, (s2, c))
       | Sequence (s1 as Syntax {nullable, first, ...}, s2) =>
            if KindSet.member (KindsCell.get first, k)
               then pierce (k, (s1, FollowBy s2 :: c))
               else pierce (k, (s2, Prepend (Option.valOf (OptionCell.get nullable)) :: c))
       | Transform (s, f) => pierce (k, (s, Apply f :: c))
       | Marked (s, _) => pierce (k, (s, c))
       | Recursive (_, s) => pierce (k, (s (), c))
       | Success _ => raise Fail "internal error"
       | Failure => raise Fail "internal error"

   val _: nonterm * layer list -> focus = plug
   val _: focus -> nonterm option = nullable
   val _: kind * focus -> focus option = locate
   val _: kind * focus -> layer list = pierce

   fun parsef (s, []) = nullable s
     | parsef (s, t :: ts) =
      let
         val k = Token.kind t
      in
         case locate (k, s) of
            NONE => NONE
          | SOME s' => parsef ((epsilon (Nonterm.token t), pierce (k, s')), ts)
      end
in
   fun parse (s, ts) = parsef (focus s, ts)
end

end

(* vim: set tw=0 sw=3 ts=3: *)
