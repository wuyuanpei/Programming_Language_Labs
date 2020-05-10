use "../dictionary/sorted_dictionary.sig";
use "../binary_tree/binary_tree.sml";
use "../dictionary/dictionary_utils.sml";

(* Richard Wu *)
structure SortedDictionary :> SORTED_DICTIONARY = struct
    type ''k compare_function = (''k*''k) -> order
    
    (* TODO: replace unit with the type you decide upon *)
    type (''k,'v) dictionary = ((''k compare_function)*((''k*'v) BinaryTree.tree ref))
    

    fun create(comp : ''k compare_function) : (''k,'v) dictionary = 
        (comp, ref BinaryTree.LEAF)

    fun get(dict : (''k,'v) dictionary, key:''k) : 'v option =
        case (BinaryTree.find (fn(ky,_) => ((#1 dict)(ky, key))) (!(#2 dict))) of
            NONE => NONE
            | SOME (_, vl) => SOME vl

    fun put(dict : (''k,'v) dictionary, key:''k, value:'v) : 'v option =
        case (BinaryTree.find (fn(ky,_) => ((#1 dict)(ky, key))) (!(#2 dict))) of
            NONE => let 
                        val _ = (#2 dict) := (BinaryTree.insert (fn((ky,_),(ky2,_)) => ((#1 dict)(ky2, ky))) (!(#2 dict)) (key, value))
                    in
                        NONE
                    end
            | SOME (_, vl) => 
                    let 
                        val t = BinaryTree.remove (fn(ky,_) => ((#1 dict)(ky, key))) (!(#2 dict))
                        val _ = (#2 dict) := (BinaryTree.insert (fn((ky,_),(ky2,_)) => ((#1 dict)(ky2, ky))) t (key, value))
                    in
                        SOME vl
                    end

    fun remove(dict : (''k,'v) dictionary, key : ''k) : 'v option =
        case (BinaryTree.find (fn(ky,_) => ((#1 dict)(ky, key))) (!(#2 dict))) of
            NONE => NONE
            | SOME (_, vl) => 
                    let 
                        val _ = (#2 dict) := BinaryTree.remove (fn(ky,_) => ((#1 dict)(ky, key))) (!(#2 dict))
                    in
                        SOME vl
                    end

    fun entries(dict : (''k,'v) dictionary) : (''k*'v) list =
        BinaryTree.fold_rnl (fn(entry,init)=>entry::init) [] (!(#2 dict))

    fun keys(dict : (''k,'v) dictionary) : ''k list = 
        entries_to_keys(entries(dict))

    fun values(dict : (''k,'v) dictionary) : 'v list = 
        entries_to_values(entries(dict))

end (* struct *) 
