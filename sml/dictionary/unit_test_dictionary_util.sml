use "../unit_test/unit_test.sml";

fun test_string_to_int_dictionary get put remove entries keys values dict =
    let
        val k = "fred"

        val _ = UnitTest.assertEquals_IntOption(NONE,   get(dict, k))
        val _ = UnitTest.assertEquals_IntOption(NONE,   put(dict, k, 2))
        val _ = UnitTest.assertEquals_IntOption(SOME 2, get(dict, k))
        val _ = UnitTest.assertEquals_IntOption(SOME 2, remove(dict, k))
        val _ = UnitTest.assertEquals_IntOption(NONE,   get(dict, k))
        val _ = UnitTest.assertEquals_IntOption(NONE,   put(dict, k, 3))
        val _ = UnitTest.assertEquals_IntOption(SOME 3, get(dict, k))
        val _ = UnitTest.assertEquals_IntOption(SOME 3, put(dict, k, 4))
        val _ = UnitTest.assertEquals_IntOption(SOME 4, get(dict, k))
        val _ = UnitTest.assertEquals_IntOption(SOME 4, put(dict, k, 5))
        val _ = UnitTest.assertEquals_IntOption(SOME 5, get(dict, k))

        val _ = UnitTest.assertEquals_StringList([k],   keys(dict))
        val _ = UnitTest.assertEquals_IntList([5],      values(dict))

        val _ = UnitTest.assertEquals_IntOption(SOME 5, remove(dict, k))
        val _ = UnitTest.assertEquals_IntOption(NONE,   get(dict, k))

        val _ = UnitTest.assertEquals_StringList([],    keys(dict))
        val _ = UnitTest.assertEquals_IntList([],       values(dict))
    in
        ()
    end

val assertEquals_StringIntList = UnitTest.assertEquals (Repr.toString o Repr.listToRepr (Repr.t2ToRepr Repr.S Repr.I))

