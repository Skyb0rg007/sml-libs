
structure Result: RESULT =
struct

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

val stdConfidence = {certainty = IntInf.pow (10, 9), tolerance = 0.9}

fun withOk (T r, ok) = T {
      ok = ok,
      expect = #expect r,
      reason = #reason r,
      theExn = #theExn r,
      abort = #abort r,
      numTests = #numTests r,
      checkCoverage = #checkCoverage r,
      labels = #labels r,
      classes = #classes r,
      tables = #tables r,
      coverage = #coverage r,
      testCase = #testCase r
   }

fun withExpect (T r, expect) = T {
      ok = #ok r,
      expect = expect,
      reason = #reason r,
      theExn = #theExn r,
      abort = #abort r,
      numTests = #numTests r,
      checkCoverage = #checkCoverage r,
      labels = #labels r,
      classes = #classes r,
      tables = #tables r,
      coverage = #coverage r,
      testCase = #testCase r
   }

fun withReason (T r, reason) = T {
      ok = #ok r,
      expect = #expect r,
      reason = reason,
      theExn = #theExn r,
      abort = #abort r,
      numTests = #numTests r,
      checkCoverage = #checkCoverage r,
      labels = #labels r,
      classes = #classes r,
      tables = #tables r,
      coverage = #coverage r,
      testCase = #testCase r
   }

fun withTheExn (T r, theExn) = T {
      ok = #ok r,
      expect = #expect r,
      reason = #reason r,
      theExn = theExn,
      abort = #abort r,
      numTests = #numTests r,
      checkCoverage = #checkCoverage r,
      labels = #labels r,
      classes = #classes r,
      tables = #tables r,
      coverage = #coverage r,
      testCase = #testCase r
   }

fun withAbort (T r, abort) = T {
      ok = #ok r,
      expect = #expect r,
      reason = #reason r,
      theExn = #theExn r,
      abort = abort,
      numTests = #numTests r,
      checkCoverage = #checkCoverage r,
      labels = #labels r,
      classes = #classes r,
      tables = #tables r,
      coverage = #coverage r,
      testCase = #testCase r
   }

fun withNumTests (T r, numTests) = T {
      ok = #ok r,
      expect = #expect r,
      reason = #reason r,
      theExn = #theExn r,
      abort = #abort r,
      numTests = numTests,
      checkCoverage = #checkCoverage r,
      labels = #labels r,
      classes = #classes r,
      tables = #tables r,
      coverage = #coverage r,
      testCase = #testCase r
   }

fun withCheckCoverage (T r, checkCoverage) = T {
      ok = #ok r,
      expect = #expect r,
      reason = #reason r,
      theExn = #theExn r,
      abort = #abort r,
      numTests = #numTests r,
      checkCoverage = checkCoverage,
      labels = #labels r,
      classes = #classes r,
      tables = #tables r,
      coverage = #coverage r,
      testCase = #testCase r
   }

fun withLabels (T r, labels) = T {
      ok = #ok r,
      expect = #expect r,
      reason = #reason r,
      theExn = #theExn r,
      abort = #abort r,
      numTests = #numTests r,
      checkCoverage = #checkCoverage r,
      labels = labels,
      classes = #classes r,
      tables = #tables r,
      coverage = #coverage r,
      testCase = #testCase r
   }

fun withClasses (T r, classes) = T {
      ok = #ok r,
      expect = #expect r,
      reason = #reason r,
      theExn = #theExn r,
      abort = #abort r,
      numTests = #numTests r,
      checkCoverage = #checkCoverage r,
      labels = #labels r,
      classes = classes,
      tables = #tables r,
      coverage = #coverage r,
      testCase = #testCase r
   }

fun withTables (T r, tables) = T {
      ok = #ok r,
      expect = #expect r,
      reason = #reason r,
      theExn = #theExn r,
      abort = #abort r,
      numTests = #numTests r,
      checkCoverage = #checkCoverage r,
      labels = #labels r,
      classes = #classes r,
      tables = tables,
      coverage = #coverage r,
      testCase = #testCase r
   }

fun withCoverage (T r, coverage) = T {
      ok = #ok r,
      expect = #expect r,
      reason = #reason r,
      theExn = #theExn r,
      abort = #abort r,
      numTests = #numTests r,
      checkCoverage = #checkCoverage r,
      labels = #labels r,
      classes = #classes r,
      tables = #tables r,
      coverage = coverage,
      testCase = #testCase r
   }

fun withTestCase (T r, testCase) = T {
      ok = #ok r,
      expect = #expect r,
      reason = #reason r,
      theExn = #theExn r,
      abort = #abort r,
      numTests = #numTests r,
      checkCoverage = #checkCoverage r,
      labels = #labels r,
      classes = #classes r,
      tables = #tables r,
      coverage = #coverage r,
      testCase = testCase
   }

val succeeded = T {
   ok = SOME true,
   expect = true,
   reason = "",
   theExn = NONE,
   abort = true,
   numTests = NONE,
   checkCoverage = NONE,
   labels = [],
   classes = [],
   tables = [],
   coverage = [],
   testCase = []
}

val failed = withOk (succeeded, SOME false)

val rejected = withOk (succeeded, NONE)

fun fromBool true = succeeded
  | fromBool false = withReason (failed, "Falsified")

fun fromExn (msg, exn) =
   let
      val reason = String.concat [msg, ":", exnMessage exn]
   in
      withReason (withTheExn (failed, SOME exn), reason)
   end

end
(* vim: set ft=sml tw=0 ts=3 sw=3: *)
