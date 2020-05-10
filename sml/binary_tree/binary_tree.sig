(* Dennis Cosgrove *)


signature BINARY_TREE = sig
    datatype 'a tree = LEAF | BRANCH of 'a tree * 'a * 'a tree

    val insert : (('a * 'a) -> order) -> ('a tree) -> 'a -> 'a tree
    val remove : ('a -> order) -> ('a tree) -> 'a tree
    val find : ('a -> order) -> ('a tree) -> 'a option

    val find_lnr : ('a -> bool) -> ('a tree) -> 'a option
    val find_rnl : ('a -> bool) -> ('a tree) -> 'a option

    val fold_lnr : (('a * 'b) -> 'b) -> ('b) -> ('a tree) -> 'b 
    val fold_rnl : (('a * 'b) -> 'b) -> ('b) -> ('a tree) -> 'b 

    val filter : ('a -> bool) -> ('a tree) -> 'a tree
    val filter_civil_war_style : ('a -> bool) -> ('a tree) -> 'a tree

    val map_to_tree_of_questionable_validity : ('a -> 'b) -> ('a tree) -> 'b tree
    val map_to_tree_lnr : (('b * 'b) -> order) -> ('a -> 'b) -> ('a tree) -> 'b tree
    val map_to_tree_rnl : (('b * 'b) -> order) -> ('a -> 'b) -> ('a tree) -> 'b tree
    val map_to_list_lnr : ('a -> 'b) -> ('a tree) -> 'b list
    val map_to_list_rnl : ('a -> 'b) -> ('a tree) -> 'b list

    val to_string : ('a -> string) -> ('a tree) -> string
end
