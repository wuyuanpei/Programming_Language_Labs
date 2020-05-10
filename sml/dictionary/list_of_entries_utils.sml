(* Richard Wu *)
use "../dictionary/list_of_entries_utils.sig";

structure ListOfEntriesUtils :> LIST_OF_ENTRIES_UTILS = struct
    exception NotYetImplemented
    
    exception KeyNotFound
    fun internal_remove(entries : (''k*'v) list, key : ''k) : 'v option * (''k * 'v) list =
        raise NotYetImplemented

    fun list_get(entries : (''k*'v) list, key : ''k) : 'v option = 
            case entries of
                [] => NONE
                |(ky,vl)::b => if ky = key then SOME vl else list_get(b, key)

    fun list_put(entries : (''k*'v) list, key : ''k, value : 'v) : 'v option * (''k * 'v) list=
        let
            fun put_v (l, acc) = 
                case l of
                    [] => (NONE,(key, value)::acc)
                    |(ky,vl)::b =>  if ky = key 
                                    then (SOME vl,((key, value)::acc)@b)
                                    else put_v(b, (ky,vl)::acc)
        in
            put_v(entries,[])
        end

    fun list_remove(entries : (''k*'v) list, key : ''k) : 'v option * (''k * 'v) list =
        let
            fun remove_v (l, acc) = 
                case l of
                    [] => (NONE, acc)
                    |(ky,vl)::b =>  if ky = key 
                                    then (SOME vl, acc@b)
                                    else remove_v(b, (ky,vl)::acc)
        in
            remove_v(entries,[])
        end
end
