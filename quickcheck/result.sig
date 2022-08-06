
signature RESULT =
sig

type confidence = {certainty: IntInf.int, tolerance: real}

datatype t = T of {
   ok: bool option,
   expect: bool,
   reason: string,
   theExn: exn option,
   abort: bool,
   numTests: int option,
   checkCoverage: confidence option,
   labels: string list,
   classes: string list,
   tables: (string * string) list,
   coverage: (string option * string * real) list,
   testCase: string list
}

val stdConfidence: confidence

(* Functional updates *)
val withOk: t * bool option -> t
val withExpect: t * bool -> t
val withReason: t * string -> t
val withTheExn: t * exn option -> t
val withAbort: t * bool -> t
val withNumTests: t * int option -> t
val withCheckCoverage: t * confidence option -> t
val withLabels: t * string list -> t
val withClasses: t * string list -> t
val withTables: t * (string * string) list -> t
val withTestCase: t * string list -> t

(* Standard constructors *)
val succeeded: t
val failed: t
val rejected: t
val fromBool: bool -> t
val fromExn: string * exn -> t

end
(* vim: set ft=sml tw=0 ts=3 sw=3: *)
