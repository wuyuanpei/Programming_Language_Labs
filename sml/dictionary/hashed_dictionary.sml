use "../dictionary/hashed_dictionary.sig";
use "../dictionary/list_of_entries_utils.sml";
use "../dictionary/dictionary_utils.sml";

(* Richard Wu *)
structure HashedDictionary :> HASHED_DICTIONARY = struct
    type ''k hash_function = ''k -> int
    
    (* TODO: replace unit with the type you decide upon *)
    type (''k,'v) dictionary = ((''k hash_function)*((''k*'v) list array))

    fun create(bucket_count_request : int, hash : ''k hash_function) : (''k,'v) dictionary = 
        (hash, Array.array(bucket_count_request,[]))

    fun positive_remainder(v : int, n : int) : int = 
        let
            val result = v mod n 
        in 
            if result >= 0 then result else result+n
        end 

    fun get_bucket_index(hash : ''k hash_function, buckets : 'a Array.array, key : ''k) : int =
        positive_remainder(hash key, Array.length(buckets))

    fun get(dict : (''k,'v) dictionary, key:''k) : 'v option =
        let
            val bucket = get_bucket_index(#1 dict, #2 dict, key)
            val l = Array.sub(#2 dict, bucket)
        in
            ListOfEntriesUtils.list_get(l, key)
        end

    fun put(dict : (''k,'v) dictionary, key:''k, value:'v) : 'v option =
        let
            val bucket = get_bucket_index(#1 dict, #2 dict, key)
            val l = Array.sub(#2 dict, bucket)
            val (res,l) = ListOfEntriesUtils.list_put(l, key, value)
            val _ = Array.update(#2 dict, bucket, l)
        in
            res
        end

    fun remove(dict : (''k,'v) dictionary, key : ''k) : 'v option =
        let
            val bucket = get_bucket_index(#1 dict, #2 dict, key)
            val l = Array.sub(#2 dict, bucket)
            val (res,l) = ListOfEntriesUtils.list_remove(l, key)
            val _ = Array.update(#2 dict, bucket, l)
        in
            res
        end

    fun entries(dict : (''k,'v) dictionary) : (''k*'v) list =
        let
            fun helper(l,init) =
                case l of
                    [] => init
                    | a::b => a::(helper(b, init))
        in
            Array.foldl helper [] (#2 dict)
        end

    fun keys(dict : (''k,'v) dictionary) : ''k list = 
        entries_to_keys(entries(dict))

    fun values(dict : (''k,'v) dictionary) : 'v list = 
        entries_to_values(entries(dict))

end (* struct *) 
