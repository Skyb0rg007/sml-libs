
structure SCGI =
struct

  fun parseHeaderSize vec =
        case Word8VectorSlice.findi (fn (_, w) => w = 0wx3A) vec of
            NONE => raise Fail "Should have header size in the first buf"
          | SOME (0, _) => raise Fail "Header size cannot be the empty string"
          | SOME (i, _) => let
              val (size, rest) = Word8VectorSlice.splitAt (vec, i)
              in
                case Int.fromString (Byte.unpackStringVec size) of
                    NONE => raise Fail "Invalid header size"
                  | SOME sz => (sz, Word8VectorSlice.triml 1 rest)
              end

  fun parseHeaders vec = let
        val headers = String.fields (fn c => c = #"\000") vec
        fun go (k :: v :: hs, acc) =
              go (hs, (k, v) :: acc)
          | go ("" :: [], acc) = acc
          | go ([], acc) = acc
          | go _ = raise Fail "Invalid header format"
        in
          List.rev (go (headers, []))
        end

  fun parseSCGI sock = let
        val vec = Word8VectorSlice.full (Socket.recvVec (sock, 2048))
        val (sz, vec) = parseHeaderSize vec
        val (headers, rest) = Word8VectorSlice.splitAt (vec, sz)
        val headers = parseHeaders (Byte.unpackStringVec headers)
        val body = Word8VectorSlice.triml 1 rest
        in
          print ("size: " ^ Int.toString sz ^ "\n");
          print ("vec: " ^ String.toString (Byte.unpackStringVec vec) ^ "\n");
          (headers, Byte.unpackStringVec body)
        end

  fun acceptLoop (passiveSock, callback) = let
        val (activeSock, _) = Socket.accept passiveSock
        val () = print "Accepted a socket connection\n"
        val (headers, body) = parseSCGI activeSock
        in
          callback (headers, body);
          Socket.close activeSock;
          print "Closed a socket connection\n";
          acceptLoop (passiveSock, callback)
        end

  fun start (filename, callback) : unit = let
        val s = UnixSock.Strm.socket ()
        val addr = UnixSock.toAddr filename
        in
          Socket.Ctl.setREUSEADDR (s, true);
          Socket.bind (s, addr);
          Socket.listen (s, 128);
          acceptLoop (s, callback)
        end
        handle e => (OS.FileSys.remove filename; raise e)

end
