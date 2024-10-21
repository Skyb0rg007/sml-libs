
structure Repair =
struct

type token = int

datatype repair
  = Insert of token
  | Delete
  | Shift

datatype repair_merge
  = Repair of repair
  | Merge of repair * repair_merge list list

datatype path = Path of {
    pstack : int list,
    laidx : int,
    repairs : repair_merge list,
    cf : int,
    cg : int
  }

fun last_repair (Path {repairs, ...}) =
  case repairs of
      [] => NONE
    | Repair r :: _ => SOME r
    | Merge (r, _) :: _ => SOME r

fun compatible (path1, path2) =
  let
    val Path {laidx = l1, pstack = p1, repairs = r1, ...} = path1
    val Path {laidx = l2, pstack = p2, repairs = r2, ...} = path2

    fun finalDelete r =
      case last_repair r of
          SOME Delete => true
        | _ => false

    fun shifts r =
      let
        fun go ([], acc) = acc
          | go (Shift :: r, acc) = go (r, acc + 1)
      in
        go (r, 0)
      end
  in
    l1 = l2
    andalso p1 = p2
    andalso finalDelete path1 = finalDelete path2
    (* andalso shifts r1 = shifts r2 *)
  end

end
