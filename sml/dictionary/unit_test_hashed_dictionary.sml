use "unit_test_dictionary_util.sml";
use "hashed_dictionary.sml";

val _ = let
    fun string_hash(x : string) = Word.toInt(HashString.hashString(x))
    (*
    fun pessimal_hash(x : 'a) : int = 0 
    *)

    val test_hashed_string_to_int_dictionary = test_string_to_int_dictionary HashedDictionary.get HashedDictionary.put HashedDictionary.remove HashedDictionary.entries HashedDictionary.keys HashedDictionary.values

    val _ = print "\n\n\n>>> begin test error output\n"
    val _ = UnitTest.enter( "HashedDictionary")
    val _ = test_hashed_string_to_int_dictionary (HashedDictionary.create(256, string_hash))
    val _ = UnitTest.leave()
    val _ = print "<<< end test error output\n\n"
in
    ()
end
