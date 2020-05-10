(* Dennis Cosgrove *)
use "../unit_test/unit_test.sml";
use "binary_tree.sml";
use "apps_using_binary_tree.sml";

val _ = print "\n\n\n>>> begin test error output\n"

val _ = let
    open BinaryTree

    fun identity(v) = v
    fun string_to_string(v) = "\"" ^ v ^ "\""

    val int_tree_to_string = BinaryTree.to_string Int.toString
    val string_tree_to_string = BinaryTree.to_string string_to_string

    val assertEquals_IntBinaryTree = UnitTest.assertEquals int_tree_to_string
    val assertEquals_StringBinaryTree = UnitTest.assertEquals string_tree_to_string


    val t_425 = BRANCH(LEAF, 425, LEAF)
    val t_231_425 = BRANCH(LEAF, 231, t_425)
    val t_231 = BRANCH(LEAF, 231, LEAF)

    val t_42 = BRANCH(LEAF, 42, LEAF)
    val t_23_42 = BRANCH(LEAF, 23, t_42)

    fun create_equals_fn(v : ''a) = 
        fn(x) => x = v


    (*
    * https://en.wikipedia.org/wiki/Tree_traversal#/media/File:Sorted_binary_tree_inorder.svg
    *)

    val t_c = BRANCH(LEAF, "C", LEAF)
    val t_e = BRANCH(LEAF, "E", LEAF)
    val t_cde = BRANCH(t_c, "D", t_e)
    val t_h = BRANCH(LEAF, "H", LEAF)
    val t_hi = BRANCH(t_h, "I", LEAF)
    val t_ghi = BRANCH(LEAF, "G", t_hi)
    val t_a = BRANCH(LEAF, "A", LEAF)
    val t_abcde = BRANCH(t_a, "B", t_cde)
    val t_abcedfghi = BRANCH(t_abcde, "F", t_ghi)

    val t_cd = BRANCH(t_c, "D", LEAF)

    val t_wiki =
        let 
            val c = BRANCH(LEAF, "C", LEAF)
            val e = BRANCH(LEAF, "E", LEAF)
            val h = BRANCH(LEAF, "H", LEAF)
            val d = BRANCH(c, "D", e)
            val i = BRANCH(h, "I", LEAF)
            val g = BRANCH(LEAF, "G", i)
            val a = BRANCH(LEAF, "A", LEAF)
            val b = BRANCH(a, "B", d)
            val f = BRANCH(b, "F", g)
        in 
            f
        end 

    val t_wiki_cut_a =
        let 
            val c = BRANCH(LEAF, "C", LEAF)
            val e = BRANCH(LEAF, "E", LEAF)
            val h = BRANCH(LEAF, "H", LEAF)
            val d = BRANCH(c, "D", e)
            val i = BRANCH(h, "I", LEAF)
            val g = BRANCH(LEAF, "G", i)
            (*val a = BRANCH(LEAF, "A", LEAF)*)
            val b = BRANCH(LEAF(*a*), "B", d)
            val f = BRANCH(b, "F", g)
        in 
            f
        end 

    val t_wiki_cut_a_and_g =
        let 
            val c = BRANCH(LEAF, "C", LEAF)
            val e = BRANCH(LEAF, "E", LEAF)
            (*val h = BRANCH(LEAF, "H", LEAF)*)
            val d = BRANCH(c, "D", e)
            (*val i = BRANCH(h, "I", LEAF)*)
            (*val g = BRANCH(LEAF, "G", i)*)
            (*val a = BRANCH(LEAF, "A", LEAF)*)
            val b = BRANCH(LEAF(*a*), "B", d)
            val f = BRANCH(b, "F", LEAF(*g*))
        in 
            f
        end 

    fun to_equal_less_greater (compare_function:('a * 'a) -> order) (v:'a) : ('a -> order) =
        fn(a) => compare_function(v,a)

    val int_equal_less_greater = to_equal_less_greater Int.compare
    val int_equal_less_greater_425 = int_equal_less_greater 425
    val int_equal_less_greater_231 = int_equal_less_greater 231
    val int_equal_less_greater_131 = int_equal_less_greater 131

    val string_equal_less_greater = to_equal_less_greater String.compare
    val string_equal_less_greater_A = string_equal_less_greater "A"
    val string_equal_less_greater_B = string_equal_less_greater "B"
    val string_equal_less_greater_C = string_equal_less_greater "C"
    val string_equal_less_greater_D = string_equal_less_greater "D"
    val string_equal_less_greater_E = string_equal_less_greater "E"
    val string_equal_less_greater_F = string_equal_less_greater "F"
    val string_equal_less_greater_G = string_equal_less_greater "G"
    val string_equal_less_greater_H = string_equal_less_greater "H"
    val string_equal_less_greater_I = string_equal_less_greater "I"

    val string_equal_less_greater_J = string_equal_less_greater "J"

    val _ = UnitTest.enter("find")
    val _ = UnitTest.assertEquals_IntOption(NONE, BinaryTree.find int_equal_less_greater_425 LEAF)
    val _ = UnitTest.assertEquals_IntOption(SOME(425), BinaryTree.find int_equal_less_greater_425 t_425)
    val _ = UnitTest.assertEquals_IntOption(NONE, BinaryTree.find int_equal_less_greater_231 LEAF)
    val _ = UnitTest.assertEquals_IntOption(NONE, BinaryTree.find int_equal_less_greater_231 t_425)
    val _ = UnitTest.assertEquals_IntOption(SOME(231), BinaryTree.find int_equal_less_greater_231 t_231)
    val _ = UnitTest.assertEquals_IntOption(SOME(231), BinaryTree.find int_equal_less_greater_231 t_231_425)
    val _ = UnitTest.assertEquals_IntOption(NONE, BinaryTree.find int_equal_less_greater_131 t_231_425)
    val _ = UnitTest.assertEquals_StringOption(NONE, BinaryTree.find string_equal_less_greater_A LEAF)
    val _ = UnitTest.assertEquals_StringOption(NONE, BinaryTree.find string_equal_less_greater_J t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("C"), BinaryTree.find string_equal_less_greater_C t_c)
    val _ = UnitTest.assertEquals_StringOption(NONE, BinaryTree.find string_equal_less_greater_C t_e)
    val _ = UnitTest.assertEquals_StringOption(SOME("C"), BinaryTree.find string_equal_less_greater_C t_cde)
    val _ = UnitTest.assertEquals_StringOption(SOME("D"), BinaryTree.find string_equal_less_greater_D t_cde)
    val _ = UnitTest.assertEquals_StringOption(SOME("E"), BinaryTree.find string_equal_less_greater_E t_cde)
    val _ = UnitTest.assertEquals_StringOption(SOME("F"), BinaryTree.find string_equal_less_greater_F t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("G"), BinaryTree.find string_equal_less_greater_G t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("B"), BinaryTree.find string_equal_less_greater_B t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("A"), BinaryTree.find string_equal_less_greater_A t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("B"), BinaryTree.find string_equal_less_greater_B t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("C"), BinaryTree.find string_equal_less_greater_C t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("D"), BinaryTree.find string_equal_less_greater_D t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("E"), BinaryTree.find string_equal_less_greater_E t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("F"), BinaryTree.find string_equal_less_greater_F t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("G"), BinaryTree.find string_equal_less_greater_G t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("H"), BinaryTree.find string_equal_less_greater_H t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME("I"), BinaryTree.find string_equal_less_greater_I t_wiki)
    val _ = UnitTest.assertEquals_StringOption(NONE, BinaryTree.find string_equal_less_greater_J t_wiki)
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("insert")
    val _ = assertEquals_IntBinaryTree(t_425, BinaryTree.insert Int.compare LEAF 425)
    val _ = assertEquals_IntBinaryTree(t_231, BinaryTree.insert Int.compare LEAF 231)
    val _ = assertEquals_IntBinaryTree(t_231_425, BinaryTree.insert Int.compare t_231 425)
    val _ = assertEquals_IntBinaryTree(t_231_425, BinaryTree.insert Int.compare t_231 425)
    val _ = assertEquals_StringBinaryTree(t_c, BinaryTree.insert String.compare LEAF "C")
    (* TODO create your own tests for insert *)
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("remove")
    val _ = assertEquals_IntBinaryTree(LEAF, BinaryTree.remove int_equal_less_greater_425 LEAF)
    val _ = assertEquals_IntBinaryTree(t_231, BinaryTree.remove int_equal_less_greater_425 t_231)
    val _ = assertEquals_IntBinaryTree(LEAF, BinaryTree.remove int_equal_less_greater_425 t_425)
    val _ = assertEquals_IntBinaryTree(t_231, BinaryTree.remove int_equal_less_greater_425 t_231_425)
    val _ = assertEquals_IntBinaryTree(t_425, BinaryTree.remove int_equal_less_greater_231 t_231_425)
    (* TODO create your own tests for remove *)
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("map_to_tree_of_questionable_validity")
    val _ = assertEquals_IntBinaryTree(LEAF, BinaryTree.map_to_tree_of_questionable_validity identity LEAF)
    val _ = assertEquals_IntBinaryTree(t_425, BinaryTree.map_to_tree_of_questionable_validity identity t_425)
    val _ = assertEquals_IntBinaryTree(t_231_425, BinaryTree.map_to_tree_of_questionable_validity identity t_231_425)
    val _ = assertEquals_StringBinaryTree(t_wiki, BinaryTree.map_to_tree_of_questionable_validity identity t_wiki)
    val _ = assertEquals_IntBinaryTree(t_42, BinaryTree.map_to_tree_of_questionable_validity (fn(x) => x div 10) t_425)
    val _ = assertEquals_IntBinaryTree(t_23_42, BinaryTree.map_to_tree_of_questionable_validity (fn(x) => x div 10) t_231_425)
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("filter_civil_war_style")
    val _ = assertEquals_IntBinaryTree(LEAF, BinaryTree.filter_civil_war_style (fn(a)=>true) LEAF)
    val _ = assertEquals_IntBinaryTree(t_231_425, BinaryTree.filter_civil_war_style (fn(a)=>true) t_231_425)
    val _ = assertEquals_IntBinaryTree(t_425, BinaryTree.filter_civil_war_style (fn(a)=>true) t_425)
    val _ = assertEquals_IntBinaryTree(t_231, BinaryTree.filter_civil_war_style (fn(a)=>a=231) t_231_425)
    val _ = assertEquals_IntBinaryTree(LEAF, BinaryTree.filter_civil_war_style (fn(a)=>a=100) t_231_425)
    val _ = assertEquals_IntBinaryTree(LEAF, BinaryTree.filter_civil_war_style (fn(a)=>a=425) t_231_425)
    val _ = assertEquals_StringBinaryTree(t_wiki_cut_a, BinaryTree.filter_civil_war_style (fn(a)=>a<>"A") t_wiki)
    val _ = assertEquals_StringBinaryTree(t_wiki_cut_a_and_g, BinaryTree.filter_civil_war_style (fn(a)=>a<>"G") t_wiki_cut_a)
    val _ = assertEquals_StringBinaryTree(t_wiki_cut_a_and_g, BinaryTree.filter_civil_war_style (fn(a)=>a<>"A" andalso a<>"G") t_wiki)
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("find_lnr")
    val equals_425 = create_equals_fn(425)
    val equals_231 = create_equals_fn(231)
    val equals_131 = create_equals_fn(131)
    val _ = UnitTest.assertEquals_IntOption(NONE,     BinaryTree.find_lnr equals_425 LEAF)
    val _ = UnitTest.assertEquals_IntOption(SOME 425, BinaryTree.find_lnr equals_425 t_425)
    val _ = UnitTest.assertEquals_IntOption(NONE,     BinaryTree.find_lnr equals_231 t_425)
    val _ = UnitTest.assertEquals_IntOption(SOME 425, BinaryTree.find_lnr equals_425 t_231_425)
    val _ = UnitTest.assertEquals_IntOption(SOME 231, BinaryTree.find_lnr equals_231 t_231_425)
    val _ = UnitTest.assertEquals_IntOption(NONE,     BinaryTree.find_lnr equals_131 t_231_425)
    val _ = UnitTest.assertEquals_StringOption(SOME "A", BinaryTree.find_lnr (create_equals_fn("A")) t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME "B", BinaryTree.find_lnr (create_equals_fn("B")) t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME "C", BinaryTree.find_lnr (create_equals_fn("C")) t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME "D", BinaryTree.find_lnr (create_equals_fn("D")) t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME "E", BinaryTree.find_lnr (create_equals_fn("E")) t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME "F", BinaryTree.find_lnr (create_equals_fn("F")) t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME "G", BinaryTree.find_lnr (create_equals_fn("G")) t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME "H", BinaryTree.find_lnr (create_equals_fn("H")) t_wiki)
    val _ = UnitTest.assertEquals_StringOption(SOME "I", BinaryTree.find_lnr (create_equals_fn("I")) t_wiki)
    val _ = UnitTest.assertEquals_StringOption(NONE,     BinaryTree.find_lnr (create_equals_fn("J")) t_wiki)
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("fold_lnr")
    val _ = UnitTest.assertEquals_String("unchanged", BinaryTree.fold_lnr (fn(x,acc)=>acc) "unchanged" LEAF)
    val _ = UnitTest.assertEquals_Int(0, BinaryTree.fold_lnr(fn(x,acc)=>acc) 0 LEAF)
    val _ = UnitTest.assertEquals_String("unchanged", BinaryTree.fold_lnr(fn(x,acc)=>acc) "unchanged" t_wiki)
    val _ = UnitTest.assertEquals_Int(0, BinaryTree.fold_lnr(fn(x,acc)=>acc) 0 t_wiki)
    val _ = UnitTest.assertEquals_String("ABCDEFGHI", BinaryTree.fold_lnr(fn(x,acc)=>acc^x) "" t_wiki)
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("max_height")
    val _ = UnitTest.assertEquals_Int(0, max_height(LEAF))
    val _ = UnitTest.assertEquals_Int(1, max_height(t_425))
    val _ = UnitTest.assertEquals_Int(2, max_height(t_231_425))
    val _ = UnitTest.assertEquals_Int(4, max_height(t_wiki))
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("sum_int_tree")
    val _ = UnitTest.assertEquals_Int(0, sum_int_tree(LEAF))
    val _ = UnitTest.assertEquals_Int(425, sum_int_tree(t_425))
    val _ = UnitTest.assertEquals_Int(656, sum_int_tree(t_231_425))
    val _ = UnitTest.leave()

    val _ = print "<<< end test error output\n\n"

in 
    () 
end