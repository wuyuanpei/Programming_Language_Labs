use "../dictionary/single_list_dictionary.sml";

(* __Richard Wu__ *)
(* Dennis Cosgrove *)
signature COMMA_SEPARATED_VALUE = sig
    val read_csv : string -> string list list
end

structure Csv :> COMMA_SEPARATED_VALUE = struct
    exception NotYetImplemented
    fun is_new_line(c : char) : bool =
        c = #"\n"

    fun is_comma(c : char) : bool =
        c = #","

    fun read_csv(csv:string) : string list list =
        let
            val rows = String.fields is_new_line csv 
            fun each_row(rows:string list) =
                case rows of
                [] => []
                | a::b => (String.fields is_comma a)::each_row(b)
        in
            each_row(rows)
        end
end

signature SPREADSHEET = sig
    datatype cell = EMPTY | TEXT of string | VALUE of int
    type sheet = cell list list

    val create_sheet : string list list -> sheet
    val row_count : sheet -> int
    val column_count : sheet -> int

    val cell_at : (sheet*int*int) -> cell

    val row_at : (sheet*int) -> cell list
    val column_at : (sheet*int) -> cell list

    val sum_values_in_row : (sheet*int) -> int
    val sum_values_in_column : (sheet*int) -> int

    val max_value_in_row : (sheet*int) -> int option
    val max_value_in_column : (sheet*int) -> int option

    val count_if_in_row : (sheet*int*(cell->bool)) -> int
    val count_if_in_column : (sheet*int*(cell->bool)) -> int

    val EXTRA_CREDIT_to_dictionaries_using_headers_as_keys : sheet -> (cell,cell) SingleListDictionary.dictionary list
end

structure Spreadsheet :> SPREADSHEET = struct
    exception NotYetImplemented

    datatype cell = EMPTY | TEXT of string | VALUE of int
    type sheet = cell list list

    exception NotYetImplemented

    fun create_sheet(word_lists : string list list) : sheet =
        let
            fun m (str) =
                case (Int.fromString str) of
                    NONE => if (String.size str) = 0 then EMPTY else TEXT str
                    | SOME a => VALUE a
            fun m2 (strss) =
                case strss of
                    [] => []
                    | a::b => (List.map m a)::m2(b)
        in
            m2 word_lists
        end

    fun row_count(s : sheet) : int =
        case s of
        [] => 0
        | a::b => 1 + row_count(b)

    fun column_count(s : sheet) : int = 
        case s of
        [] => 0
        |_ => List.length (hd s)

    fun row_at(s : sheet, row_index : int) : cell list = 
        if row_index = 0
        then hd s
        else row_at(tl s, row_index-1)

    fun cell_in_row_at_column_index( r : cell list, col_index : int) : cell = 
        if col_index = 0
        then hd r
        else cell_in_row_at_column_index (tl r, col_index-1)

    fun cell_at(s : sheet, row_index : int, col_index : int) : cell = 
        cell_in_row_at_column_index(row_at(s, row_index), col_index)

    fun column_at(s : sheet, col_index : int) : cell list =
        case s of
        [] => []
        | a::b => cell_in_row_at_column_index(a,col_index):: column_at(b, col_index)

    fun sum_values_in_cell_list(cells : cell list) : int =
        case cells of
            [] => 0
            | a::b => (case a of
                        VALUE x => x + sum_values_in_cell_list(b)
                        | _ => sum_values_in_cell_list(b))

    fun sum_values_in_row(s : sheet, row_index : int) : int =
        sum_values_in_cell_list(row_at(s, row_index))

    fun sum_values_in_column(s : sheet, column_index : int) : int =
        sum_values_in_cell_list(column_at(s, column_index))

    fun max_value_in_cell_list(cells : cell list) : int option =
        case cells of
            [] => NONE
            |(VALUE x)::b=> (case max_value_in_cell_list(b) of
                                NONE => SOME x
                                | SOME v => if x > v then SOME x else SOME v)
            | _::b => max_value_in_cell_list(b)

    fun max_value_in_row(s : sheet, row_index : int) : int option =
        max_value_in_cell_list(row_at(s, row_index))

    fun max_value_in_column(s : sheet, column_index : int) : int option =
        max_value_in_cell_list(column_at(s, column_index))

    fun count_if_in_cell_list(cells : cell list, predicate : (cell -> bool)) : int = 
        case cells of
        [] => 0
        | a::b => if predicate a then 1 + count_if_in_cell_list(b, predicate) else count_if_in_cell_list(b, predicate)

    fun count_if_in_row(s : sheet, row_index : int, predicate : (cell -> bool)) : int = 
        count_if_in_cell_list(row_at(s, row_index), predicate)

    fun count_if_in_column(s : sheet, col_index : int, predicate : (cell -> bool)) : int = 
        count_if_in_cell_list(column_at(s, col_index), predicate)

    fun EXTRA_CREDIT_to_dictionaries_using_headers_as_keys(s : sheet) : (cell,cell) SingleListDictionary.dictionary list =
        let
        val hds = row_at(s,0)
        fun map (cs: cell list) : (cell,cell) SingleListDictionary.dictionary =
            let
                val dic = SingleListDictionary.create()
                fun add (cs, hds) =
                    case cs of
                    [] => dic
                    | a::b => (let 
                                    val _ = SingleListDictionary.put(dic,hd hds, a) 
                                in  
                                    add(b, tl hds) 
                                end)
            in
                add (cs, hds)
            end

        fun map_dict(s: sheet) =
            case s of
            [] => []
            | a::b => map(a)::map_dict(b)

        in
            case s of
            [] => []
            | a::b => map_dict(b)
        end    
end
