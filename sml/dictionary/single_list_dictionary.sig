use "../dictionary/dictionary.sig";

(* __Richard Wu__ *)

signature SINGLE_LIST_DICTIONARY = sig include DICTIONARY
    val create : unit -> (''k,'v) dictionary
end
