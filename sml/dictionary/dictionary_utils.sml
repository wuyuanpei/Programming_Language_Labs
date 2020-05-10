(* Richard Wu *)
exception NotYetImplemented

fun entries_to_keys(entries : (''k*'v) list) : ''k list = 
    List.foldl (fn ((key,_),init)=> key::init) [] entries

fun entries_to_values(entries : (''k*'v) list) : 'v list =
    List.foldl (fn ((_,value),init)=> value::init) [] entries
