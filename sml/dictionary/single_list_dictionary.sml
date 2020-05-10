use "../dictionary/single_list_dictionary.sig";
use "../dictionary/list_of_entries_utils.sml";
use "../dictionary/dictionary_utils.sml";

(* Richard Wu *)

structure SingleListDictionary :> SINGLE_LIST_DICTIONARY = struct
    
    (* TODO: replace unit with the type you decide upon *)
    type (''k,'v) dictionary = (''k*'v) list ref

    fun create() : (''k,'v) dictionary = 
        ref []

    fun get(dict : (''k,'v) dictionary, key:''k) : 'v option =
        ListOfEntriesUtils.list_get(!dict, key)

    fun put(dict : (''k,'v) dictionary, key:''k, value:'v) : 'v option =
        let
            val (res,l) = ListOfEntriesUtils.list_put(!dict, key, value)
            val _ = dict := l
        in
            res
        end


    fun remove(dict : (''k,'v) dictionary, key : ''k) : 'v option =
        let
            val (res,l) = ListOfEntriesUtils.list_remove(!dict, key)
            val _ = dict := l
        in
            res
        end

    fun entries(dict : (''k,'v) dictionary) : (''k*'v) list =
        !dict

    fun keys(dict : (''k,'v) dictionary) : ''k list = 
        entries_to_keys(entries(dict))

    fun values(dict : (''k,'v) dictionary) : 'v list = 
        entries_to_values(entries(dict))

end (* struct *) 
