
structure RealConversions =
struct
  val real64ToWord64 = MLton.Real64.castToWord
  val word64ToReal64 = MLton.Real64.castFromWord
end
