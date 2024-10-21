
structure Souffle =
struct

(* val dlopen : string option * bool * bool -> Word32.word = *)
(*   Unsafe.CInterface.c_function "UNIX-Dynload" "dlopen" *)

(* val dlsym : Word32.word * string -> Word32.word = *)
(*   Unsafe.CInterface.c_function "UNIX-Dynload" "dlsym" *)

(* val dlerror : unit -> string option = *)
(*   Unsafe.CInterface.c_function "UNIX-Dynload" "dlerror" *)

(* val dlclose : Word32.word -> unit = *)
(*   Unsafe.CInterface.c_function "UNIX-Dynload" "dlclose" *)

datatype direction =
    Input
  | Output
  | InputOutput
  | Internal

datatype push = Push of {
    string : string -> unit,
    real64 : Real64.real -> unit,
    int32 : Int32.int -> unit,
    word32 : Word32.word -> unit
  }

datatype pop = Pop of {
    string : unit -> string,
    real64 : unit -> Real64.real,
    int32 : unit -> Int32.int,
    word32 : unit -> Word32.word
  }

datatype 'a fact = Fact of {
    name : string,
    direction : direction,
    push : push * 'a -> unit,
    pop : pop -> 'a
  }

datatype hdl = Handle of {
    soufflePath : string,
    tmpDirPath : string,
    factPath : string,
    outputPath : string,
    datalogExec : string,
    numThreads : int
  }

end
