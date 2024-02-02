
structure JSONLexer =
struct

datatype state =
   BASE
 | STRING
 | NUMBER
 | TRUE
 | FALSE
 | NULL

datatype res_type =
   RES_NUMBER
 | RES_STRING
 | RES_TRUE
 | RES_FALSE
 | RES_NULL
 | RES_OBJSTART
 | RES_OBJEND
 | RES_ARRSTART
 | RES_ARREND
 | RES_STRING_PARTIAL
 | RES_NUMBER_PARTIAL
 | RES_NUMBER_SMALL

datatype result = Result of {
      restype : res_type,
      startpos : int,
      length : int,
      adddata : bool
   }

datatype lexer = Lexer of {
      state : state ref,
      data : int ref,
      pos : int ref,
      len : int ref,
      res_num : int ref,
      res_lim : int ref
   }

end

(* vim: set ts=3 sw=3: *)
