signature LIST_OF_ENTRIES_UTILS = sig
    val list_get : (''k * 'v) list * ''k -> 'v option
    val list_put : (''k * 'v) list * ''k * 'v -> 'v option * (''k * 'v) list
    val list_remove : (''k * 'v) list * ''k -> 'v option * (''k * 'v) list
end
