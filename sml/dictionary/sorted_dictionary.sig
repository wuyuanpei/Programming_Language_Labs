use "../dictionary/dictionary.sig";

(* Dennis Cosgrove *)
signature SORTED_DICTIONARY = sig include DICTIONARY
    type ''k compare_function = (''k*''k) -> order
    val create : ''k compare_function -> (''k,'v) dictionary
end
