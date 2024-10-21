
structure Mustache =
struct

datatype token =
    Text of string
  | Variable of {escape: bool, name: string}
  | SectionOpen of {invert: bool, tag: string}
  | SectionClose of string
  | Partial of string
  | DelimiterChange of {openTag: string, closeTag: string}

fun tokenize template =
  let
    fun go s =
      ()
  in
    ()
  end

end
