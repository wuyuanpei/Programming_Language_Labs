(* Dennis Cosgrove *)
use "core_unit_test_binary_tree.sml";

signature TEST_INSERT_FIND_REMOVE = sig
    val test_insert : unit -> unit
    val test_remove : unit -> unit
    val test_find : unit -> unit
end

structure TestInsertFindRemove :> TEST_INSERT_FIND_REMOVE = struct
    open UnitTest
    open BinaryTree
    open CoreUnitTestBinaryTree

    fun test_insert() =
        ( enter("insert")
        (* NOTE: assertInsertAll relies on uniqueSort <<<*)
        ; assertInsertAll_Int([231, 425])
        ; assertInsertAll_Int([425, 231])
        ; assertInsertAll_Int([131, 231, 425])
        ; assertInsertAll_Int([231, 131, 425])
        ; assertInsertAll_Int([425, 231, 131])
        ; assertInsertAll_Int([42, 425, 231, 131])
        ; assertInsertAll_Int([425, 131, 231, 42, 9, 6, 12, 4, 20, 7])
        ; assertInsertAll_String(["truth", "is", "beauty"])
        ; assertInsertAll_String(["four", "score", "and", "seven", "years", "ago"])

        ; assertInsertAllInRandomOrderRepeatedly_Int(11, [0,1,2,3,4,5,6,7,8,9])
        ; assertInsertAllInRandomOrderRepeatedly_String(11, ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"])
        ; leave()
        )

    fun test_find() =
        let
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
        in
            ( enter("find")
            ; assertEquals_IntOption(NONE, BinaryTree.find int_equal_less_greater_425 LEAF)
            ; assertEquals_IntOption(SOME(425), BinaryTree.find int_equal_less_greater_425 t_425)
            ; assertEquals_IntOption(NONE, BinaryTree.find int_equal_less_greater_231 LEAF)
            ; assertEquals_IntOption(NONE, BinaryTree.find int_equal_less_greater_231 t_425)
            ; assertEquals_IntOption(SOME(231), BinaryTree.find int_equal_less_greater_231 t_231)
            ; assertEquals_IntOption(SOME(231), BinaryTree.find int_equal_less_greater_231 t_231_425)
            ; assertEquals_IntOption(NONE, BinaryTree.find int_equal_less_greater_131 t_231_425)
            ; assertEquals_StringOption(NONE, BinaryTree.find string_equal_less_greater_A LEAF)
            ; assertEquals_StringOption(NONE, BinaryTree.find string_equal_less_greater_J t_wiki)
            ; assertEquals_StringOption(SOME("C"), BinaryTree.find string_equal_less_greater_C t_c)
            ; assertEquals_StringOption(NONE, BinaryTree.find string_equal_less_greater_C t_e)
            ; assertEquals_StringOption(SOME("C"), BinaryTree.find string_equal_less_greater_C t_cde)
            ; assertEquals_StringOption(SOME("D"), BinaryTree.find string_equal_less_greater_D t_cde)
            ; assertEquals_StringOption(SOME("E"), BinaryTree.find string_equal_less_greater_E t_cde)
            ; assertEquals_StringOption(SOME("F"), BinaryTree.find string_equal_less_greater_F t_wiki)
            ; assertEquals_StringOption(SOME("G"), BinaryTree.find string_equal_less_greater_G t_wiki)
            ; assertEquals_StringOption(SOME("B"), BinaryTree.find string_equal_less_greater_B t_wiki)
            ; assertEquals_StringOption(SOME("A"), BinaryTree.find string_equal_less_greater_A t_wiki)
            ; assertEquals_StringOption(SOME("B"), BinaryTree.find string_equal_less_greater_B t_wiki)
            ; assertEquals_StringOption(SOME("C"), BinaryTree.find string_equal_less_greater_C t_wiki)
            ; assertEquals_StringOption(SOME("D"), BinaryTree.find string_equal_less_greater_D t_wiki)
            ; assertEquals_StringOption(SOME("E"), BinaryTree.find string_equal_less_greater_E t_wiki)
            ; assertEquals_StringOption(SOME("F"), BinaryTree.find string_equal_less_greater_F t_wiki)
            ; assertEquals_StringOption(SOME("G"), BinaryTree.find string_equal_less_greater_G t_wiki)
            ; assertEquals_StringOption(SOME("H"), BinaryTree.find string_equal_less_greater_H t_wiki)
            ; assertEquals_StringOption(SOME("I"), BinaryTree.find string_equal_less_greater_I t_wiki)
            ; assertEquals_StringOption(NONE, BinaryTree.find string_equal_less_greater_J t_wiki)
            ; leave()
            )
        end

    fun test_remove() =
        ( enter("remove")
        (* NOTE: assertInsertAll relies on uniqueSort <<<*)
        ; assertInsertAllInOrderFollowedByRemove_Int( [2,1,3], 3)
        ; assertInsertAllInOrderFollowedByRemove_Int( [2,1,3], 1)
        ; assertInsertAllInOrderFollowedByRemove_Int( [2,1,3], 2)

        ; assertInsertAllInOrderFollowedByRemove_String( ["F","B","A","D","C","E","G","I","H"], "A")
        ; assertInsertAllInOrderFollowedByRemove_String( ["F","B","A","D","C","E","G","I","H"], "C")
        ; assertInsertAllInOrderFollowedByRemove_String( ["F","B","A","D","C","E","G","I","H"], "E")
        ; assertInsertAllInOrderFollowedByRemove_String( ["F","B","A","D","C","E","G","I","H"], "H")
        ; assertInsertAllInOrderFollowedByRemove_String( ["F","B","A","D","C","E","G","I","H"], "I")
        ; assertInsertAllInOrderFollowedByRemove_String( ["F","B","A","D","C","E","G","I","H"], "G")
        ; assertInsertAllInOrderFollowedByRemove_String( ["F","B","A","D","C","E","G","I","H"], "D")
        ; assertInsertAllInOrderFollowedByRemove_String( ["F","B","A","D","C","E","G","I","H"], "B")
        ; assertInsertAllInOrderFollowedByRemove_String( ["F","B","A","D","C","E","G","I","H"], "F")

        ; assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrderRepeatedly_Int(11, [0,1,2,3,4,5,6,7,8,9])
        ; assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrderRepeatedly_String(11, ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"])
        ; leave()
        )
end

val _ = ( UnitTest.processCommandLineArgs()
        ; TestInsertFindRemove.test_insert()
        ; TestInsertFindRemove.test_find()
        ; TestInsertFindRemove.test_remove()
        ; OS.Process.exit(OS.Process.success)
        )