(* Dennis Cosgrove *)
use "../unit_test/unit_test.sml";
use "hw1.sml";

val _ = let
    val _ = print "\n\n\n>>> begin test error output\n"

    val _ = UnitTest.enter("is_older")
    val _ = UnitTest.assertTrue(is_older((1,2,3),(2,3,4)))
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("number_in_month")
    val _ = UnitTest.assertEquals_Int(1, number_in_month([(2012,2,28),(2013,12,1)],2))
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("number_in_months")
    val _ = UnitTest.assertEquals_Int(3, number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]))
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("dates_in_month")
    val _ = UnitTest.assertEquals_IntIntIntList([(2012,2,28)], dates_in_month ([(2012,2,28),(2013,12,1)],2))
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("dates_in_months")
    val _ = UnitTest.assertEquals_IntIntIntList([(2012,2,28),(2011,3,31),(2011,4,28)], dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]))
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("get_nth")
    val _ = UnitTest.assertEquals_String("there", get_nth (["hi", "there", "how", "are", "you"], 2))
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("date_to_string")
    val _ = UnitTest.assertEquals_String("June 1, 2013", date_to_string (2013, 6, 1))
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("number_before_reaching_sum")
    val _ = UnitTest.assertEquals_Int(3, number_before_reaching_sum (10, [1,2,3,4,5,6,7,8]))
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("what_month_tests")
    val _ = UnitTest.assertEquals_Int(3, what_month (70))
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("month_range")
    val _ = UnitTest.assertEquals_IntList([1,2,2,2], month_range (31, 34))
    val _ = UnitTest.leave()

    val _ = UnitTest.enter("oldest")
    val _ = UnitTest.assertEquals_IntIntIntOption(SOME (2011,3,31), oldest([(2012,2,28),(2011,3,31),(2011,4,28)]))
    val _ = UnitTest.leave()

    val _ = print "<<< end test error output\n\n"
in
	()
end
