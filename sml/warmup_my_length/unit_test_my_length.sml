(* Dennis Cosgrove *)
use "../unit_test/unit_test.sml";
use "my_length.sml";

val _ = let
    val _ = print "\n\n\n>>> begin test error output\n"

    val _ = UnitTest.enter("length")
    val _ = UnitTest.assertEquals_Int(0, my_length([]))
    val _ = UnitTest.assertEquals_Int(1, my_length([425]))
    val _ = UnitTest.assertEquals_Int(2, my_length([425, 231]))
    val _ = UnitTest.assertEquals_Int(2, my_length(["PL", "Parallel"]))
    val _ = UnitTest.assertEquals_Int(4, my_length(["A", "B", "C", "D"]))
    val _ = UnitTest.leave()

    val _ = print "<<< end test error output\n\n"
in
	()
end
