(* TODO: use CM.make to avoid having to go up and down a directory??? *)
use "../repr/repr.sig";

structure Repr :> REPR = struct
  datatype 'a repr = 
      B of bool 
    | C of char
    | I of int 
    | R of real 
    | S of string
    | QUOTED_STRING of string
    | T2 of 'a repr * 'a repr 
    | T3 of 'a repr * 'a repr * 'a repr 
    | T4 of 'a repr * 'a repr * 'a repr  * 'a repr 
    | OPT of 'a repr option 
    | REF of 'a repr ref 
    | LIST of 'a repr list
    | CUSTOM of ('a->'a repr) * 'a

  fun toString(v : 'a repr) : string =
    case v of
        B(b) => Bool.toString(b)
      | C(c) => Char.toString(c)
      | I(i) => Int.toString(i)
      | R(r) => Real.toString(r)
      | QUOTED_STRING(s) => "\"" ^ s ^ "\""
      | S(s) => s
      | T2(a,b) => "(" ^ toString(a) ^ ", " ^ toString(b) ^ ")"
      | T3(a,b,c) => "(" ^ toString(a) ^ ", " ^ toString(b) ^ ", " ^ toString(c) ^ ")"
      | T4(a,b,c,d) => "(" ^ toString(a) ^ ", " ^ toString(b) ^ ", " ^ toString(c) ^ ", " ^ toString(d) ^ ")"
      | OPT(NONE) => "NONE"
      | OPT(SOME(v')) => "SOME(" ^ toString(v') ^ ")"
      | REF(ref(v')) => "ref(" ^ toString(v') ^ ")"
      | LIST(xs) => 
        let
          fun helper(xs) : string =
            case xs of
                [] => ""
              | last::[] => toString(last)
              | head::neck::rest => toString(head) ^ ", " ^ helper(neck::rest)
        in
          "[" ^ helper(xs) ^ "]"
        end
      | CUSTOM(f, v') => toString(f(v'))

  fun optToRepr f opt =
    case opt of 
        NONE => OPT(NONE)
      | SOME(v) => OPT(SOME(f(v)))

  fun listToRepr f xs =
    LIST(List.map f xs)

  fun t2ToRepr fa fb (a,b) =
    T2(fa(a), fb(b))

  fun t3ToRepr fa fb fc (a,b,c) =
    T3(fa(a), fb(b), fc(c))

  fun t4ToRepr fa fb fc fd (a,b,c,d) =
    T4(fa(a), fb(b), fc(c), fd(d))

  fun refToRepr f r =
    REF(ref(f(!r)))
end
