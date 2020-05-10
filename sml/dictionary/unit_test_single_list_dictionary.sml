use "unit_test_dictionary_util.sml";
use "single_list_dictionary.sml";

val _ = let
    val test_single_list_string_to_int_dictionary = test_string_to_int_dictionary SingleListDictionary.get SingleListDictionary.put SingleListDictionary.remove SingleListDictionary.entries SingleListDictionary.keys SingleListDictionary.values

    val _ = print "\n\n\n>>> begin test error output\n"
    val _ = UnitTest.enter( "SingleListDictionary")
    val _ = test_single_list_string_to_int_dictionary (SingleListDictionary.create())
    val _ = UnitTest.leave()
    val _ = print "<<< end test error output\n\n"
in
    ()
end
