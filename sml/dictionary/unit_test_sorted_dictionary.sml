use "unit_test_dictionary_util.sml";
use "sorted_dictionary.sml";

val _ = let
    val _ = print "\n\n\n>>> begin test error output\n"

    val test_sorted_string_to_int_dictionary = test_string_to_int_dictionary SortedDictionary.get SortedDictionary.put SortedDictionary.remove SortedDictionary.entries SortedDictionary.keys SortedDictionary.values
    val dict : (string,int) SortedDictionary.dictionary = SortedDictionary.create(String.compare)
    val _ = UnitTest.enter("SortedDictionary")

    val _ = test_sorted_string_to_int_dictionary (SortedDictionary.create(String.compare))

    val _ = assertEquals_StringIntList([], SortedDictionary.entries(dict))

    val _ = SortedDictionary.put(dict, "C", 3)
    val _ = SortedDictionary.put(dict, "A", 1)
    val _ = SortedDictionary.put(dict, "E", 5)
    val _ = SortedDictionary.put(dict, "D", 4)
    val _ = SortedDictionary.put(dict, "B", 2)
    val _ = UnitTest.assertEquals_StringList(["A", "B", "C", "D", "E"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntList([1,2,3,4,5], SortedDictionary.values(dict))
    val _ = SortedDictionary.put(dict, "I", 9)
    val _ = SortedDictionary.put(dict, "F", 6)
    val _ = SortedDictionary.put(dict, "H", 8)
    val _ = SortedDictionary.put(dict, "G", 7)
    val _ = UnitTest.assertEquals_StringList(["A", "B", "C", "D", "E", "F", "G", "H", "I"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntList([1,2,3,4,5,6,7,8,9], SortedDictionary.values(dict))

    val _ = UnitTest.assertEquals_IntOption(SOME 6, SortedDictionary.remove(dict, "F"))
    val _ = UnitTest.assertEquals_StringList(["A", "B", "C", "D", "E", "G", "H", "I"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntOption(SOME 1, SortedDictionary.remove(dict, "A"))
    val _ = UnitTest.assertEquals_StringList(["B", "C", "D", "E", "G", "H", "I"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntOption(SOME 4, SortedDictionary.remove(dict, "D"))
    val _ = UnitTest.assertEquals_StringList(["B", "C", "E", "G", "H", "I"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntOption(SOME 5, SortedDictionary.remove(dict, "E"))
    val _ = UnitTest.assertEquals_StringList(["B", "C", "G", "H", "I"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntOption(SOME 2, SortedDictionary.remove(dict, "B"))
    val _ = UnitTest.assertEquals_StringList(["C", "G", "H", "I"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntOption(SOME 9, SortedDictionary.remove(dict, "I"))
    val _ = UnitTest.assertEquals_StringList(["C", "G", "H"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntOption(SOME 7, SortedDictionary.remove(dict, "G"))
    val _ = UnitTest.assertEquals_StringList(["C", "H"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntOption(SOME 8, SortedDictionary.remove(dict, "H"))
    val _ = UnitTest.assertEquals_StringList(["C"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntOption(SOME 3, SortedDictionary.remove(dict, "C"))
    val _ = UnitTest.assertEquals_StringList([], SortedDictionary.keys(dict))

    val _ = SortedDictionary.put(dict, "E", 5)
    val _ = SortedDictionary.put(dict, "C", 3)
    val _ = SortedDictionary.put(dict, "G", 7)
    val _ = SortedDictionary.put(dict, "A", 1)
    val _ = SortedDictionary.put(dict, "B", 2)
    val _ = SortedDictionary.put(dict, "D", 4)
    val _ = SortedDictionary.put(dict, "F", 6)
    val _ = SortedDictionary.put(dict, "H", 8)
    val _ = SortedDictionary.put(dict, "I", 9)
    val _ = UnitTest.assertEquals_IntOption(SOME 5, SortedDictionary.remove(dict, "E"))
    val _ = UnitTest.assertEquals_StringList(["A", "B", "C", "D", "F", "G", "H", "I"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntOption(SOME 3, SortedDictionary.remove(dict, "C"))
    val _ = UnitTest.assertEquals_StringList(["A", "B", "D", "F", "G", "H", "I"], SortedDictionary.keys(dict))
    val _ = UnitTest.assertEquals_IntOption(SOME 7, SortedDictionary.remove(dict, "G"))
    val _ = UnitTest.assertEquals_StringList(["A", "B", "D", "F", "H", "I"], SortedDictionary.keys(dict))

    val _ = UnitTest.leave()

    val _ = print "<<< end test error output\n\n"
in
    ()
end
