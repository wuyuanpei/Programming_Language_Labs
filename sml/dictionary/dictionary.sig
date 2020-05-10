(* Dennis Cosgrove *)

signature DICTIONARY = sig
    type (''k,'v) dictionary
    val get : ((''k,'v) dictionary *''k) -> 'v option
    val put : ((''k,'v) dictionary *''k *'v) -> 'v option
    val remove : ((''k,'v) dictionary *''k) -> 'v option
    val entries : (''k,'v) dictionary -> (''k*'v) list
    val keys : (''k,'v) dictionary -> ''k list
    val values : (''k,'v) dictionary -> 'v list
end
