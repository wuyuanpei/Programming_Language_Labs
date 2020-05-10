(* Dennis Cosgrove *)
use "core_unit_test_binary_tree.sml";

val _ = print "\n\n\n>>> begin test error output\n"

val _ = let
    open CoreUnitTestBinaryTree

    val _ = UnitTest.enter("insert >>> NOTE: relies on uniqueSort <<<")
    val _ = assertInsertAll_Int([231, 425])
    val _ = assertInsertAll_Int([425, 231])
    val _ = assertInsertAll_Int([131, 231, 425])
    val _ = assertInsertAll_Int([231, 131, 425])
    val _ = assertInsertAll_Int([425, 231, 131])
    val _ = assertInsertAll_Int([42, 425, 231, 131])
    val _ = assertInsertAll_Int([425, 131, 231, 42, 9, 6, 12, 4, 20, 7])
    val _ = assertInsertAll_String(["truth", "is", "beauty"])
    val _ = assertInsertAll_String(["four", "score", "and", "seven", "years", "ago"])
    val _ = UnitTest.leave()

    val _ = print "<<< end test error output\n\n"
in 
    () 
end