signature REPR = sig
    datatype 'a repr
        = B of bool
        | C of char
        | CUSTOM of ('a -> 'a repr) * 'a
        | I of int
        | LIST of 'a repr list
        | OPT of 'a repr option
        | QUOTED_STRING of string        
        | R of real
        | REF of 'a repr ref
        | S of string
        | T2 of 'a repr * 'a repr
        | T3 of 'a repr * 'a repr * 'a repr
        | T4 of 'a repr * 'a repr * 'a repr * 'a repr

    val toString : 'a repr -> string
    val optToRepr : ('a -> 'b repr) -> 'a option -> 'b repr
    val listToRepr : ('a -> 'b repr) -> 'a list -> 'b repr
    val t2ToRepr : ('a -> 'b repr) -> ('c -> 'b repr) -> 'a * 'c -> 'b repr
    val t3ToRepr : ('a -> 'b repr) -> ('c -> 'b repr) -> ('d -> 'b repr) -> 'a * 'c * 'd -> 'b repr
    val t4ToRepr : ('a -> 'b repr) -> ('c -> 'b repr) -> ('d -> 'b repr) -> ('e -> 'b repr) -> 'a * 'c * 'd * 'e -> 'b repr
    val refToRepr : ('a -> 'b repr) -> 'a ref -> 'b repr
end
