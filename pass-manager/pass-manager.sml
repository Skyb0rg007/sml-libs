
structure CPS =
   struct
      datatype exp = Exp
   end

structure FactManager:
   sig
      type t

      val register: Atom.atom * (JSON.value * CPS.exp -> Universal.universal) -> unit
      val get: t * Atom.atom -> Universal.universal
      val new: CPS.exp * JSON.value -> t
   end =
   struct
      type generator = JSON.value * CPS.exp -> Universal.universal

      val emptyTag: unit Universal.tag = Universal.tag ()
      val empty = Universal.tagInject emptyTag ()
      val isEmpty = Universal.tagIs emptyTag

      val tbl: generator AtomTable.hash_table =
         AtomTable.mkTable (16, Fail "Invalid fact name")

      datatype t = T of {
            exp: CPS.exp,
            opts: JSON.value AtomMap.map,
            cache: Universal.universal AtomTable.hash_table
         }

      fun new (exp, opts) =
         T {
            exp = exp,
            opts =
               case opts of
                  JSON.NULL => AtomMap.empty
                | JSON.OBJECT obj =>
                     List.foldl
                        (fn ((k, v), m) =>
                           AtomMap.insertWithi
                              (fn (k, _, _) => raise Fail ("Duplicate keys: " ^ Atom.toString k))
                              (m, Atom.atom k, v))
                        AtomMap.empty
                        obj
                | _ => raise Fail "Invalid options",
            cache = AtomTable.mkTable (16, Fail "FactManager: internal error")
         }

      fun register (name, run) =
         if AtomTable.inDomain tbl name
            then raise Fail "Duplicate fact names!"
            else AtomTable.insert tbl (name, run)

      fun get (T {exp, opts, cache}, name) =
         let
            val run = AtomTable.lookup tbl name
         in
            case AtomTable.find cache name of
               NONE =>
                  let
                     val opt = AtomMap.find (opts, name)
                     val data = run (Option.getOpt (opt, JSON.NULL), exp)
                  in
                     AtomTable.insert cache (name, data);
                     data
                  end
             | SOME data => data
         end
   end

structure CFAFact:
   sig
      val name: Atom.atom
      val lookup: FactManager.t * Atom.atom -> Atom.atom list
   end =
   struct
      val name = Atom.atom "CFA"
      type data = Atom.atom list AtomTable.hash_table
      val tag: data Universal.tag = Universal.tag ()
      val inj = Universal.tagInject tag
      val prj = Universal.tagProject tag

      fun run (opts, CPS.Exp) =
         let
            val tbl = AtomTable.mkTable (16, Fail "")
         in
            AtomTable.insert tbl (Atom.atom "foo", [Atom.atom "bar"]);
            tbl
         end

      val () = FactManager.register (name, inj o run)

      fun lookup (manager, key) =
         Option.getOpt (AtomTable.find (prj (FactManager.get (manager, name))) key, [])
   end

structure PassManager:
   sig
      datatype preserved =
         All
       | Preserved of Atom.atom list

      type t
      type pass_handle

      val register: string -> (JSON.value * CPS.exp -> Universal.universal) -> pass_handle
   end =
   struct
      datatype preserved =
         All
       | Preserved of Atom.atom list

      type pass = {run: JSON.value * CPS.exp -> Universal.universal}

      type pass_handle = int

      datatype t = T of {
            tbl: pass AtomTable.hash_table
         }

      fun register _ = raise Fail ""


   end

(*structure JSON = *)
(*   struct *)
(*      datatype schema = Schema *)
(*      datatype value = Value *)
(*   end *)

(*structure CPS = *)
(*   struct *)
(*      datatype exp = Exp *)
(*   end *)

(*structure Stamp = *)
(*   struct *)
(*      datatype t = T of int *)

(*      structure Tbl = HashTableFn( *)
(*         struct *)
(*            type hash_key = t *)

(*            fun hashVal (T n) = Word.fromInt n *)
(*            fun sameKey (T a, T b) = a = b *)
(*         end) *)
(*   end *)

(*structure PM = *)
(*   struct *)
(*      val epoch = ref 0 *)
(*   end *)

(*signature GENERATOR = *)
(*   sig *)
(*      type context *)
(*      type info *)

(*      val name: Atom.atom *)
(*      val options: JSON.schema *)
(*      val compute: JSON.value * CPS.exp -> context *)
(*      val lookup: context * Stamp.t -> info *)
(*      val update: context * Stamp.t * info -> unit *)
(*   end *)

(*signature FACT = *)
(*   sig *)
(*      (1* Public API *1) *)
(*      type info *)

(*      val info: Stamp.t -> info *)
(*      val update: Stamp.t * info -> unit *)

(*      (1* Private API *1) *)
(*      type gen = {name: Atom.atom, compute: JSON.value * CPS.exp -> exn, lookup: exn * Stamp.t -> info, update: exn * Stamp.t * info -> unit} *)
(*      val register: gen -> unit *)
(*      val computeWith: Atom.atom * JSON.value * CPS.exp -> unit *)
(*      val revalidate: unit -> unit *)
(*   end *)

(*functor RegisterGenerator( *)
(*      structure Fact: FACT *)
(*      structure Gen: GENERATOR where type info = Fact.info) = *)
(*   struct *)
(*      exception E of Gen.context *)
(*      fun get (E ctx) = ctx *)
(*        | get _ = raise Fail "Wrong context" *)

(*      val gen = { *)
(*            name = Gen.name, *)
(*            lookup = fn (e, s) => Gen.lookup (get e, s), *)
(*            update = fn (e, s, i) => Gen.update (get e, s, i), *)
(*            compute = fn (opts, exp) => E (Gen.compute (opts, exp)) *)
(*         } *)

(*      val () = Fact.register gen *)
(*   end *)

(*functor MakeFact(type info): FACT where type info = info = *)
(*   struct *)
(*      type info = info *)
(*      type gen = {name: Atom.atom, compute: JSON.value * CPS.exp -> exn, lookup: exn * Stamp.t -> info, update: exn * Stamp.t * info -> unit} *)

(*      val epoch = ref 0 *)
(*      val context = ref Empty *)
(*      fun undef _ = raise Fail "Undefined" *)
(*      val current: gen ref = ref {name=Atom.atom "", compute=undef, lookup=undef, update=undef} *)
(*      val gens: gen AtomTable.hash_table = AtomTable.mkTable (10, Fail "") *)

(*      fun register g = AtomTable.insert gens (#name g, g) *)

(*      fun computeWith (name, opts, exp) = *)
(*         (epoch := !PM.epoch; *)
(*          current := AtomTable.lookup gens name; *)
(*          context := #compute (!current) (opts, exp)) *)

(*      fun revalidate () = *)
(*         epoch := !PM.epoch *)

(*      fun info stamp = *)
(*         if !epoch <> !PM.epoch *)
(*            then raise Fail "Outdated information" *)
(*         else *)
(*            case !context of *)
(*               Empty => raise Fail "Information has not yet been computed" *)
(*             | ctx => #lookup (!current) (ctx, stamp) *)

(*      fun update (stamp, info) = *)
(*         if !epoch <> !PM.epoch *)
(*            then raise Fail "Outdated information" *)
(*         else *)
(*            case !context of *)
(*               Empty => raise Fail "Information has not yet been computed" *)
(*             | ctx => #update (!current) (ctx, stamp, info) *)
(*   end *)

(*structure Fact1 = MakeFact(type info = string) *)

(*structure GatherFact1A: GENERATOR where type info = Fact1.info = *)
(*   struct *)
(*      type context = string Stamp.Tbl.hash_table *)
(*      type info = string *)

(*      val name = Atom.atom "gather_fact1_a" *)
(*      val options = JSON.Schema *)

(*      fun compute _ = *)
(*         let *)
(*            val tbl = Stamp.Tbl.mkTable (10, Fail "") *)
(*         in *)
(*            Stamp.Tbl.insert tbl (Stamp.T 0, "zero"); *)
(*            Stamp.Tbl.insert tbl (Stamp.T 1, "one"); *)
(*            tbl *)
(*         end *)

(*      fun lookup (tbl, k) = *)
(*         case Stamp.Tbl.find tbl k of *)
(*            NONE => "<??>" *)
(*          | SOME s => s *)

(*      fun update (tbl, k, v) = *)
(*         Stamp.Tbl.insert tbl (k, v) *)
(*   end *)

(*structure X = RegisterGenerator( *)
(*   structure Fact = Fact1 *)
(*   structure Gen = GatherFact1A) *)

(* vim: set tw=0 sw=3 ts=3: *)
