use "../dictionary/dictionary.sig";

(* Dennis Cosgrove *)

signature HASHED_DICTIONARY = sig include DICTIONARY
    type ''k hash_function = ''k -> int
    val create : (int * ''k hash_function) -> (''k,'v) dictionary
end
