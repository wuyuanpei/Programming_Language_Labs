use "../unit_test/unit_test.sml";
use "../dictionary/single_list_dictionary.sml";
use "../dictionary/hashed_dictionary.sml";
use "../dictionary/sorted_dictionary.sml";

signature TEST_DICTIONARY_COMPREHENSIVE = sig
    val test_single : unit -> unit
    val test_hash : unit -> unit
    val test_sorted : unit -> unit
end

structure TestDictionaryComprehensive :> TEST_DICTIONARY_COMPREHENSIVE = struct
    open UnitTest

    val assertEquals_StringIntList = UnitTest.assertEquals (Repr.toString o Repr.listToRepr (Repr.t2ToRepr Repr.S Repr.I))

    fun test_string_to_int_dictionary get put remove entries keys values dict =
        let
            val k = "fred"
        in
            ( assertEquals_IntOption(NONE, get(dict, k))
            ; assertEquals_IntOption(NONE, put(dict, k, 2))
            ; assertEquals_IntOption(SOME 2, get(dict, k))
            ; assertEquals_IntOption(SOME 2, remove(dict, k))
            ; assertEquals_IntOption(NONE, get(dict, k))
            ; assertEquals_IntOption(NONE,  put(dict, k, 3))
            ; assertEquals_IntOption(SOME 3, get(dict, k))
            ; assertEquals_IntOption(SOME 3, put(dict, k, 4))
            ; assertEquals_IntOption(SOME 4, get(dict, k))
            ; assertEquals_IntOption(SOME 4, put(dict, k, 5))
            ; assertEquals_IntOption(SOME 5, get(dict, k))

            ; assertEquals_StringIntList([(k,5)], entries(dict))
            ; assertEquals_StringList([k], keys(dict))
            ; assertEquals_IntList([5], values(dict))

            ; assertEquals_IntOption(SOME 5, remove(dict, k))
            ; assertEquals_IntOption(NONE, get(dict, k))

            ; assertEquals_StringIntList([], entries(dict))
            ; assertEquals_StringList([], keys(dict))
            ; assertEquals_IntList([], values(dict))

            ; assertEquals_IntOption(NONE, put(dict, "Ted", 9))
            ; assertEquals_IntOption(SOME 9, get(dict, "Ted"))
            ; assertEquals_StringIntList([("Ted", 9)], entries(dict))
            ; assertEquals_StringList(["Ted"], keys(dict))
            ; assertEquals_IntList([9], values(dict))

            ; assertEquals_IntOption(NONE, put(dict, "Jackie", 42))
            ; assertEquals_IntOption(SOME 42, get(dict, "Jackie"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42], values(dict))

            ; assertEquals_IntOption(NONE, put(dict, "Bobby", 4))
            ; assertEquals_IntOption(SOME 4, get(dict, "Bobby"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie", "Bobby"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42, 4], values(dict))

            ; assertEquals_IntOption(NONE, put(dict, "Bill", 6))
            ; assertEquals_IntOption(SOME 6, get(dict, "Bill"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie", "Bobby", "Bill"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42, 4, 6], values(dict))

            ; assertEquals_IntOption(NONE, put(dict, "Michael", 23))
            ; assertEquals_IntOption(SOME 23, get(dict, "Michael"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie", "Bobby", "Bill", "Michael"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42, 4, 6, 23], values(dict))

            ; assertEquals_IntOption(SOME 23, remove(dict, "Michael"))
            ; assertEquals_IntOption(NONE, get(dict, "Michael"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie", "Bobby", "Bill"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42, 4, 6], values(dict))
            
            ; assertEquals_IntOption(NONE, remove(dict, "Michael"))
            ; assertEquals_IntOption(NONE, get(dict, "Michael"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie", "Bobby", "Bill"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42, 4, 6], values(dict))

            ; assertEquals_IntOption(NONE, put(dict, "Michael", 45))
            ; assertEquals_IntOption(SOME 45, get(dict, "Michael"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie", "Bobby", "Bill", "Michael"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42, 4, 6, 45], values(dict))

            ; assertEquals_IntOption(SOME 45, put(dict, "Michael", 23))
            ; assertEquals_IntOption(SOME 23, get(dict, "Michael"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie", "Bobby", "Bill", "Michael"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42, 4, 6, 23], values(dict))

            ; assertEquals_IntOption(NONE, get(dict, "Tom"))
            ; assertEquals_IntOption(NONE, put(dict, "Tom", 10))
            ; assertEquals_IntOption(SOME 10, get(dict, "Tom"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie", "Bobby", "Bill", "Michael", "Tom"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42, 4, 6, 23, 10], values(dict))

            ; assertEquals_IntOption(SOME 10, get(dict, "Tom"))
            ; assertEquals_IntOption(SOME 10, put(dict, "Tom", 12))
            ; assertEquals_IntOption(SOME 12, get(dict, "Tom"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie", "Bobby", "Bill", "Michael", "Tom"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42, 4, 6, 23, 12], values(dict))

            ; assertEquals_IntOption(SOME 6, remove(dict, "Bill"))
            ; assertEquals_IntOption(NONE, get(dict, "Bill"))
            ; assertEquals_IntOption(NONE, remove(dict, "Bill"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie", "Bobby", "Michael", "Tom"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42, 4, 23, 12], values(dict))

            ; assertEquals_IntOption(SOME 4, remove(dict, "Bobby"))
            ; assertEquals_IntOption(NONE, get(dict, "Bobby"))
            ; assertEquals_IntOption(NONE, remove(dict, "Bobby"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Jackie", "Michael", "Tom"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 42, 23, 12], values(dict))

            ; assertEquals_IntOption(SOME 42, remove(dict, "Jackie"))
            ; assertEquals_IntOption(NONE, get(dict, "Jackie"))
            ; assertEquals_IntOption(NONE, remove(dict, "Jackie"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Michael", "Tom"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 23, 12], values(dict))

            ; assertEquals_IntOption(SOME 23, remove(dict, "Michael"))
            ; assertEquals_IntOption(NONE, get(dict, "Michael"))
            ; assertEquals_IntOption(NONE, remove(dict, "Michael"))
            ; assertEqualsAnyOrder_StringList(["Ted", "Tom"], keys(dict))
            ; assertEqualsAnyOrder_IntList([9, 12], values(dict))

            ; assertEquals_IntOption(SOME 9, remove(dict, "Ted"))
            ; assertEquals_IntOption(NONE, get(dict, "Ted"))
            ; assertEquals_IntOption(NONE, remove(dict, "Ted"))
            ; assertEquals_StringIntList([("Tom", 12)], entries(dict))
            ; assertEqualsAnyOrder_StringList(["Tom"], keys(dict))
            ; assertEqualsAnyOrder_IntList([12], values(dict))

            ; assertEquals_IntOption(SOME 12, remove(dict, "Tom"))
            ; assertEquals_IntOption(NONE, get(dict, "Tom"))
            ; assertEquals_IntOption(NONE, remove(dict, "Tom"))
            ; assertEquals_StringIntList([], entries(dict))
            ; assertEquals_StringList([], keys(dict))
            ; assertEquals_IntList([], values(dict))
            )
        end


    fun test_single() =
        let
            val test_single_list_string_to_int_dictionary = test_string_to_int_dictionary SingleListDictionary.get SingleListDictionary.put SingleListDictionary.remove SingleListDictionary.entries SingleListDictionary.keys SingleListDictionary.values
        in
            ( enter( "SingleListDictionary")
            ; test_single_list_string_to_int_dictionary (SingleListDictionary.create())
            ; leave()
        )
        end

    fun test_hash() =
        let
            fun string_hash(x : string) = 
                Word.toIntX(HashString.hashString(x))
            val test_hashed_string_to_int_dictionary = test_string_to_int_dictionary HashedDictionary.get HashedDictionary.put HashedDictionary.remove HashedDictionary.entries HashedDictionary.keys HashedDictionary.values
        in
            ( enter( "HashedDictionary")
            ; test_hashed_string_to_int_dictionary (HashedDictionary.create(256, string_hash))
            ; leave()
        )
        end

    fun test_sorted() =
        let
            val test_sorted_string_to_int_dictionary = test_string_to_int_dictionary SortedDictionary.get SortedDictionary.put SortedDictionary.remove SortedDictionary.entries SortedDictionary.keys SortedDictionary.values
            val dict : (string,int) SortedDictionary.dictionary = SortedDictionary.create(String.compare)
        in
            ( enter("SortedDictionary")

            ; test_sorted_string_to_int_dictionary (SortedDictionary.create(String.compare))

            ; assertEquals_StringIntList([], SortedDictionary.entries(dict))

            ; SortedDictionary.put(dict, "C", 3)
            ; SortedDictionary.put(dict, "A", 1)
            ; SortedDictionary.put(dict, "E", 5)
            ; SortedDictionary.put(dict, "D", 4)
            ; SortedDictionary.put(dict, "B", 2)
            ; assertEquals_StringList(["A", "B", "C", "D", "E"], SortedDictionary.keys(dict))
            ; assertEquals_IntList([1,2,3,4,5], SortedDictionary.values(dict))
            ; SortedDictionary.put(dict, "I", 9)
            ; SortedDictionary.put(dict, "F", 6)
            ; SortedDictionary.put(dict, "H", 8)
            ; SortedDictionary.put(dict, "G", 7)
            ; assertEquals_StringList(["A", "B", "C", "D", "E", "F", "G", "H", "I"], SortedDictionary.keys(dict))
            ; assertEquals_IntList([1,2,3,4,5,6,7,8,9], SortedDictionary.values(dict))

            ; assertEquals_IntOption(SOME 6, SortedDictionary.remove(dict, "F"))
            ; assertEquals_StringList(["A", "B", "C", "D", "E", "G", "H", "I"], SortedDictionary.keys(dict))
            ; assertEquals_IntOption(SOME 1, SortedDictionary.remove(dict, "A"))
            ; assertEquals_StringList(["B", "C", "D", "E", "G", "H", "I"], SortedDictionary.keys(dict))
            ; assertEquals_IntOption(SOME 4, SortedDictionary.remove(dict, "D"))
            ; assertEquals_StringList(["B", "C", "E", "G", "H", "I"], SortedDictionary.keys(dict))
            ; assertEquals_IntOption(SOME 5, SortedDictionary.remove(dict, "E"))
            ; assertEquals_StringList(["B", "C", "G", "H", "I"], SortedDictionary.keys(dict))
            ; assertEquals_IntOption(SOME 2, SortedDictionary.remove(dict, "B"))
            ; assertEquals_StringList(["C", "G", "H", "I"], SortedDictionary.keys(dict))
            ; assertEquals_IntOption(SOME 9, SortedDictionary.remove(dict, "I"))
            ; assertEquals_StringList(["C", "G", "H"], SortedDictionary.keys(dict))
            ; assertEquals_IntOption(SOME 7, SortedDictionary.remove(dict, "G"))
            ; assertEquals_StringList(["C", "H"], SortedDictionary.keys(dict))
            ; assertEquals_IntOption(SOME 8, SortedDictionary.remove(dict, "H"))
            ; assertEquals_StringList(["C"], SortedDictionary.keys(dict))
            ; assertEquals_IntOption(SOME 3, SortedDictionary.remove(dict, "C"))
            ; assertEquals_StringList([], SortedDictionary.keys(dict))

            ; SortedDictionary.put(dict, "E", 5)
            ; SortedDictionary.put(dict, "C", 3)
            ; SortedDictionary.put(dict, "G", 7)
            ; SortedDictionary.put(dict, "A", 1)
            ; SortedDictionary.put(dict, "B", 2)
            ; SortedDictionary.put(dict, "D", 4)
            ; SortedDictionary.put(dict, "F", 6)
            ; SortedDictionary.put(dict, "H", 8)
            ; SortedDictionary.put(dict, "I", 9)
            ; assertEquals_IntOption(SOME 5, SortedDictionary.remove(dict, "E"))
            ; assertEquals_StringList(["A", "B", "C", "D", "F", "G", "H", "I"], SortedDictionary.keys(dict))
            ; assertEquals_IntOption(SOME 3, SortedDictionary.remove(dict, "C"))
            ; assertEquals_StringList(["A", "B", "D", "F", "G", "H", "I"], SortedDictionary.keys(dict))
            ; assertEquals_IntOption(SOME 7, SortedDictionary.remove(dict, "G"))
            ; assertEquals_StringList(["A", "B", "D", "F", "H", "I"], SortedDictionary.keys(dict))

            ; leave()
        )
        end

end

val _ = ( UnitTest.processCommandLineArgs()

        ; if CommandLineArgs.getBoolOrDefault("single", true)
          then TestDictionaryComprehensive.test_single()
          else ()

        ; if CommandLineArgs.getBoolOrDefault("hash", true)
          then TestDictionaryComprehensive.test_hash()
          else ()

        ; if CommandLineArgs.getBoolOrDefault("sorted", true)
          then TestDictionaryComprehensive.test_sorted()
          else ()

        ; OS.Process.exit(OS.Process.success)
        )
