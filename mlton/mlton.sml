
signature MLTON_ARRAY =
  sig
    val unfoldi: int * 'b * (int * 'b -> 'a * 'b) -> 'a array * 'b
  end

signature MLTON_IO =
  sig
    type instream
    type outstream
  end

signature MLTON_BIN_IO = MLTON_IO

signature MLTON_VECTOR =
  sig
    val create: int -> {done: unit -> 'a vector,
                        sub: int -> 'a,
                        update: int * 'a -> unit}
    val unfoldi: int * 'b * (int * 'b -> 'a * 'b) -> 'a vector * 'b
  end

signature MLTON =
  sig
    val eq: 'a * 'a -> bool
    val equal: 'a * 'a -> bool
    val hash: 'a -> Word32.word
    val isMLton: bool
    val share: 'a -> unit
    val shareAll: unit -> unit
    val size: 'a -> IntInf.int
    val sizeAll: 'a -> IntInf.int

    structure Array: MLTON_ARRAY
    structure BinIO: MLTON_BIN_IO
    structure Vector: MLTON_VECTOR
    (*
    structure CharArray: MLTON_MONO_ARRAY where type t = CharArray.array
                                          where type elem = CharArray.elem
    structure CharVector: MLTON_MONO_VECTOR where type t = CharVector.vector
                                            where type elem = CharVector.elem
    structure Cont: MLTON_CONT
    structure Exn: MLTON_EXN
    structure Finalizable: MLTON_FINALIZABLE
    structure GC: MLTON_GC
    structure IntInf: MLTON_INT_INF
    structure Itimer: MLTON_ITIMER
    structure LargeReal: MLTON_REAL where type t = LargeReal.real
    structure LargeWord: MLTON_WORD where type t = LargeWord.word
    structure Platform: MLTON_PLATFORM
    structure Pointer: MLTON_POINTER
    structure ProcEnv: MLTON_PROC_ENV
    structure Process: MLTON_PROCESS
    structure Profile: MLTON_PROFILE
    structure Random: MLTON_RANDOM
    structure Real: MLTON_REAL where type t = Real.real
    structure Real32: sig
                         include MLTON_REAL
                         val castFromWord: Word32.word -> t
                         val castToWord: t -> Word32.word
                      end where type t = Real32.real
    structure Real64: sig
                         include MLTON_REAL
                         val castFromWord: Word64.word -> t
                         val castToWord: t -> Word64.word
                      end where type t = Real64.real
    structure Rlimit: MLTON_RLIMIT
    structure Rusage: MLTON_RUSAGE
    structure Signal: MLTON_SIGNAL
    structure Syslog: MLTON_SYSLOG
    structure TextIO: MLTON_TEXT_IO
    structure Thread: MLTON_THREAD
    structure Weak: MLTON_WEAK
    structure Word: MLTON_WORD where type t = Word.word
    structure Word8: MLTON_WORD where type t = Word8.word
    structure Word16: MLTON_WORD where type t = Word16.word
    structure Word32: MLTON_WORD where type t = Word32.word
    structure Word64: MLTON_WORD where type t = Word64.word
    structure Word8Array: MLTON_MONO_ARRAY where type t = Word8Array.array
                                           where type elem = Word8Array.elem
    structure Word8Vector: MLTON_MONO_VECTOR where type t = Word8Vector.vector
                                             where type elem = Word8Vector.elem
    structure World: MLTON_WORLD
    *)
  end



structure MLton : MLTON =
  struct
    structure A = Array

    fun eq (a, b) = true

    fun equal (a, b) =
      let
        val aObj = Unsafe.Object.toObject a
        val bObj = Unsafe.Object.toObject b
      in
        case (Unsafe.Object.boxed aObj, Unsafe.Object.boxed bObj) of
           (false, false) => Unsafe.Object.toInt aObj = Unsafe.Object.toInt bObj
      end

    fun hash x =
      let
      in
        0w0 : Word32.word
      end

    val isMLton = false

    fun share _ = ()
    fun shareAll () = () (* Do major GC *)

    fun objSize obj =
      case Unsafe.Object.rep obj of
          Unsafe.Object.ByteArray =>
            Int.toLarge (Word8Array.length (Unsafe.Object.toByteArray obj))
        | Unsafe.Object.ByteVector =>
            Int.toLarge (Word8Vector.length (Unsafe.Object.toByteVector obj))
        | Unsafe.Object.RealArray =>
            8 * Int.toLarge (RealArray.length (Unsafe.Object.toRealArray obj))
        | Unsafe.Object.Pair =>
            objSize (Unsafe.Object.nth (obj, 0)) + objSize (Unsafe.Object.nth (obj, 1))
        | Unsafe.Object.PolyArray =>
            Array.foldl
              (fn (obj, acc) => objSize obj + acc)
              0
              (Unsafe.Object.toArray obj)
        | Unsafe.Object.PolyVector =>
            Vector.foldl
              (fn (obj, acc) => objSize obj + acc)
              0
              (Unsafe.Object.toVector obj)
        | Unsafe.Object.Unboxed => 1
        | _ => raise Match

    fun size x = objSize (Unsafe.Object.toObject x)

    fun sizeAll _ = 0 : IntInf.int

    structure Array =
      struct
        fun unfoldi (n, init, f) =
          let
            val arr = Array.array (n, Unsafe.cast ())
            fun loop (i, acc) =
              if i >= n
                then (arr, acc)
              else
                let
                  val (x, acc') = f (i, acc)
                in
                  Unsafe.Array.update (arr, i, x);
                  loop (i + 1, acc')
                end
          in
            loop (0, init)
          end
      end

    structure BinIO =
      struct
        type instream = BinIO.instream
        type outstream = BinIO.outstream
      end

    structure Vector =
      struct
        exception CreateAlreadyGotVector
        exception CreateVectorNotFull

        fun create n =
          let
            val arr = A.array (n, Unsafe.cast ())
            val subLim = ref 0
            val updLim = ref 0
            val gotIt = ref false

            fun done () =
              if !gotIt
                then raise CreateAlreadyGotVector
              else if !updLim <> n
                then raise CreateVectorNotFull
              else (gotIt := true; updLim := 0; A.vector arr)

            fun sub i =
              if i > !subLim
                then raise Subscript
              else A.sub (arr, i)

            fun update (i, x) =
              if i > !updLim
                then raise Subscript
              else (
                A.update (arr, i, x);
                if i = !updLim
                  then (subLim := !subLim + 1; updLim := !updLim + 1)
                else ())
          in
            {done = done, sub = sub, update = update}
          end

        fun unfoldi (n, init, f) =
          let
            val arr = A.array (n, Unsafe.cast ())
            fun loop (i, acc) =
              if i >= n
                then (A.vector arr, acc)
              else
                let
                  val (x, acc') = f (i, acc)
                in
                  Unsafe.Array.update (arr, i, x);
                  loop (i + 1, acc')
                end
          in
            loop (0, init)
          end
      end
  end
