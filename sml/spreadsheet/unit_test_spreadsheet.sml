(* Dennis Cosgrove *)
use "../unit_test/unit_test.sml";
use "spreadsheet.sml";

val _ = let
    open Csv
    open Spreadsheet
    open UnitTest
    val assertEquals_StringListList = assertEquals (Repr.toString o Repr.listToRepr (Repr.listToRepr Repr.QUOTED_STRING))

    fun cell_to_string c =
        case c of 
             EMPTY => "EMPTY"
        |  TEXT(s) => "TEXT(\"" ^ s ^ "\")"
        | VALUE(v) => "VALUE(" ^ Int.toString(v) ^ ")"

    val assertEquals_Cell = assertEquals (Repr.toString o (Repr.S o cell_to_string))
    val assertEquals_CellList = assertEquals (Repr.toString o (Repr.listToRepr (Repr.S o cell_to_string)))
    val assertEquals_CellListList = assertEquals (Repr.toString o Repr.listToRepr (Repr.listToRepr (Repr.S o cell_to_string)))
    val assertEquals_CellOption = assertEquals (Repr.toString o Repr.optToRepr (Repr.S o cell_to_string))



    val _ = print "\n\n\n>>> begin test error output\n"

    val _ = enter("read_csv")

    (* 
     * NOTE: very reasonable implementations of read_csv can come up with different results for "" so I have opted not to test it at all 
     *)

    val nums_csv = "1,2,3,4\n10,20,30,40"
    val nums_string_list_list = [["1", "2", "3", "4"], ["10", "20", "30", "40"]]
    val _ = assertEquals_StringListList(nums_string_list_list, read_csv(nums_csv))

    val grades_csv = "Name,Java List,SML Calendar,SML Hearts,SML Card Game,Java HOF,SML Binary Tree,SML Pattern Matching\nMax,100,104,100,104,100,100,105\nJoshua Bloch,100,85,80,75,100,70,65\nHarry Q. Bovik,80,81,82,83,84,85,86\nDan Grossman,75,104,100,104,80,100,105\nShannon O'Ganns,70,40,0,120,120,130,140"
    val grades_string_list_list = [["Name", "Java List", "SML Calendar", "SML Hearts", "SML Card Game", "Java HOF", "SML Binary Tree", "SML Pattern Matching"], ["Max", "100", "104", "100", "104", "100", "100", "105"], ["Joshua Bloch", "100", "85", "80", "75", "100", "70", "65"], ["Harry Q. Bovik", "80", "81", "82", "83", "84", "85", "86"], ["Dan Grossman", "75", "104", "100", "104", "80", "100", "105"], ["Shannon O'Ganns", "70", "40", "0", "120", "120", "130", "140"]]
    val _ = assertEquals_StringListList(grades_string_list_list, read_csv(grades_csv))

    val hockey_csv = "Name,Uniform Number,Birth Year,Games Played,Goals,Assists\nBobby Orr,4,1948,657,270,645\nWayne Gretzky,99,1961,1487,894,1963\nMario Lemieux,66,1965,915,690,1033"
    val hockey_string_list_list = [["Name", "Uniform Number", "Birth Year", "Games Played", "Goals", "Assists"], ["Bobby Orr", "4", "1948", "657", "270", "645"], ["Wayne Gretzky", "99", "1961", "1487", "894", "1963"], ["Mario Lemieux", "66", "1965", "915", "690", "1033"]]
    val _ = assertEquals_StringListList(hockey_string_list_list, read_csv(hockey_csv))

    val _ = assertEquals_StringListList([["1"]], read_csv("1"))
    val _ = assertEquals_StringListList([["1"],["2"]], read_csv("1\n2"))
    val _ = assertEquals_StringListList([["",""]], read_csv(","))
    val _ = assertEquals_StringListList([["name","rank","serial"],["Dwight David Eisenhower","General","5"],["Vasily Zaitsev","Sniper","225"], ["Grace Hopper", "Rear Admiral", "1952"]], read_csv("name,rank,serial\nDwight David Eisenhower,General,5\nVasily Zaitsev,Sniper,225\nGrace Hopper,Rear Admiral,1952"))
    val _ = assertEquals_StringListList([["1","2","3","4"]], read_csv("1,2,3,4"))

    val _ = leave()

    val nums_spreadsheet = [[VALUE(1), VALUE(2), VALUE(3), VALUE(4)], [VALUE(10), VALUE(20), VALUE(30), VALUE(40)]]
    val grades_spreadsheet = [[TEXT("Name"), TEXT("Java List"), TEXT("SML Calendar"), TEXT("SML Hearts"), TEXT("SML Card Game"), TEXT("Java HOF"), TEXT("SML Binary Tree"), TEXT("SML Pattern Matching")], [TEXT("Max"), VALUE(100), VALUE(104), VALUE(100), VALUE(104), VALUE(100), VALUE(100), VALUE(105)], [TEXT("Joshua Bloch"), VALUE(100), VALUE(85), VALUE(80), VALUE(75), VALUE(100), VALUE(70), VALUE(65)], [TEXT("Harry Q. Bovik"), VALUE(80), VALUE(81), VALUE(82), VALUE(83), VALUE(84), VALUE(85), VALUE(86)], [TEXT("Dan Grossman"), VALUE(75), VALUE(104), VALUE(100), VALUE(104), VALUE(80), VALUE(100), VALUE(105)], [TEXT("Shannon O'Ganns"), VALUE(70), VALUE(40), VALUE(0), VALUE(120), VALUE(120), VALUE(130), VALUE(140)]]
    val hockey_spreadsheet = [[TEXT("Name"), TEXT("Uniform Number"), TEXT("Birth Year"), TEXT("Games Played"), TEXT("Goals"), TEXT("Assists")], [TEXT("Bobby Orr"), VALUE(4), VALUE(1948), VALUE(657), VALUE(270), VALUE(645)], [TEXT("Wayne Gretzky"), VALUE(99), VALUE(1961), VALUE(1487), VALUE(894), VALUE(1963)], [TEXT("Mario Lemieux"), VALUE(66), VALUE(1965), VALUE(915), VALUE(690), VALUE(1033)]]

    val _ = enter("create_sheet")
    val _ = assertEquals_CellListList([], create_sheet([]))
    val _ = assertEquals_CellListList([[EMPTY]], create_sheet([[""]]))
    val _ = assertEquals_CellListList([[VALUE(1)]], create_sheet([["1"]]))
    val _ = assertEquals_CellListList([[TEXT("fred")]], create_sheet([["fred"]]))
    val _ = assertEquals_CellListList([[TEXT("fred"),TEXT("george")]], create_sheet([["fred", "george"]]))
    val _ = assertEquals_CellListList(nums_spreadsheet, create_sheet(nums_string_list_list))
    val _ = assertEquals_CellListList(grades_spreadsheet, create_sheet(grades_string_list_list))
    val _ = assertEquals_CellListList(hockey_spreadsheet, create_sheet(hockey_string_list_list))
    val _ = leave()

    val _ = enter("row_count")
    val _ = assertEquals_Int(0, row_count([]))
    val _ = assertEquals_Int(2, row_count(nums_spreadsheet))
    val _ = assertEquals_Int(6, row_count(grades_spreadsheet))
    val _ = assertEquals_Int(4, row_count(hockey_spreadsheet))
    val _ = leave()

    val _ = enter("column_count")
    val _ = assertEquals_Int(0, column_count([]))
    val _ = assertEquals_Int(4, column_count(nums_spreadsheet))
    val _ = assertEquals_Int(8, column_count(grades_spreadsheet))
    val _ = assertEquals_Int(6, column_count(hockey_spreadsheet))
    val _ = leave()

    val _ = enter("cell_at")
    val _ = assertEquals_Cell(TEXT("Name"), cell_at(hockey_spreadsheet, 0, 0))
    val _ = assertEquals_Cell(TEXT("Uniform Number"), cell_at(hockey_spreadsheet, 0, 1))
    val _ = assertEquals_Cell(TEXT("Birth Year"), cell_at(hockey_spreadsheet, 0, 2))
    val _ = assertEquals_Cell(TEXT("Games Played"), cell_at(hockey_spreadsheet, 0, 3))
    val _ = assertEquals_Cell(TEXT("Goals"), cell_at(hockey_spreadsheet, 0, 4))
    val _ = assertEquals_Cell(TEXT("Assists"), cell_at(hockey_spreadsheet, 0, 5))

    val _ = assertEquals_Cell(TEXT("Bobby Orr"), cell_at(hockey_spreadsheet, 1, 0))
    val _ = assertEquals_Cell(VALUE(4), cell_at(hockey_spreadsheet, 1, 1))
    val _ = assertEquals_Cell(VALUE(1948), cell_at(hockey_spreadsheet, 1, 2))
    val _ = assertEquals_Cell(VALUE(657), cell_at(hockey_spreadsheet, 1, 3))
    val _ = assertEquals_Cell(VALUE(270), cell_at(hockey_spreadsheet, 1, 4))
    val _ = assertEquals_Cell(VALUE(645), cell_at(hockey_spreadsheet, 1, 5))
    val _ = leave()

    val _ = enter("row_at")
    val _ = assertEquals_CellList( [VALUE(1), VALUE(2), VALUE(3), VALUE(4)], row_at(nums_spreadsheet, 0))
    val _ = assertEquals_CellList( [VALUE(10), VALUE(20), VALUE(30), VALUE(40)], row_at(nums_spreadsheet, 1))

    val _ = assertEquals_CellList( [TEXT("Name"), TEXT("Java List"), TEXT("SML Calendar"), TEXT("SML Hearts"), TEXT("SML Card Game"), TEXT("Java HOF"), TEXT("SML Binary Tree"), TEXT("SML Pattern Matching")], row_at(grades_spreadsheet, 0))
    val _ = assertEquals_CellList( [TEXT("Max"), VALUE(100), VALUE(104), VALUE(100), VALUE(104), VALUE(100), VALUE(100), VALUE(105)], row_at(grades_spreadsheet, 1))
    val _ = assertEquals_CellList( [TEXT("Joshua Bloch"), VALUE(100), VALUE(85), VALUE(80), VALUE(75), VALUE(100), VALUE(70), VALUE(65)], row_at(grades_spreadsheet, 2))
    val _ = assertEquals_CellList( [TEXT("Harry Q. Bovik"), VALUE(80), VALUE(81), VALUE(82), VALUE(83), VALUE(84), VALUE(85), VALUE(86)], row_at(grades_spreadsheet, 3))
    val _ = assertEquals_CellList( [TEXT("Dan Grossman"), VALUE(75), VALUE(104), VALUE(100), VALUE(104), VALUE(80), VALUE(100), VALUE(105)], row_at(grades_spreadsheet, 4))
    val _ = assertEquals_CellList( [TEXT("Shannon O'Ganns"), VALUE(70), VALUE(40), VALUE(0), VALUE(120), VALUE(120), VALUE(130), VALUE(140)], row_at(grades_spreadsheet, 5))

    val _ = assertEquals_CellList( [TEXT("Name"), TEXT("Uniform Number"), TEXT("Birth Year"), TEXT("Games Played"), TEXT("Goals"), TEXT("Assists")], row_at(hockey_spreadsheet, 0))
    val _ = assertEquals_CellList( [TEXT("Bobby Orr"), VALUE(4), VALUE(1948), VALUE(657), VALUE(270), VALUE(645)], row_at(hockey_spreadsheet, 1))
    val _ = assertEquals_CellList( [TEXT("Wayne Gretzky"), VALUE(99), VALUE(1961), VALUE(1487), VALUE(894), VALUE(1963)], row_at(hockey_spreadsheet, 2))
    val _ = assertEquals_CellList( [TEXT("Mario Lemieux"), VALUE(66), VALUE(1965), VALUE(915), VALUE(690), VALUE(1033)], row_at(hockey_spreadsheet, 3))
    val _ = leave()

    val _ = enter("column_at")
    val _ = assertEquals_CellList([VALUE(1), VALUE(10)], column_at(nums_spreadsheet, 0))
    val _ = assertEquals_CellList([VALUE(2), VALUE(20)], column_at(nums_spreadsheet, 1))
    val _ = assertEquals_CellList([VALUE(3), VALUE(30)], column_at(nums_spreadsheet, 2))
    val _ = assertEquals_CellList([VALUE(4), VALUE(40)], column_at(nums_spreadsheet, 3))

    val _ = assertEquals_CellList([TEXT("Name"), TEXT("Max"), TEXT("Joshua Bloch"), TEXT("Harry Q. Bovik"), TEXT("Dan Grossman"), TEXT("Shannon O'Ganns")], column_at(grades_spreadsheet, 0))
    val _ = assertEquals_CellList([TEXT("Java List"), VALUE(100), VALUE(100), VALUE(80), VALUE(75), VALUE(70)], column_at(grades_spreadsheet, 1))
    val _ = assertEquals_CellList([TEXT("SML Calendar"), VALUE(104), VALUE(85), VALUE(81), VALUE(104), VALUE(40)], column_at(grades_spreadsheet, 2))
    val _ = assertEquals_CellList([TEXT("SML Hearts"), VALUE(100), VALUE(80), VALUE(82), VALUE(100), VALUE(0)], column_at(grades_spreadsheet, 3))
    val _ = assertEquals_CellList([TEXT("SML Card Game"), VALUE(104), VALUE(75), VALUE(83), VALUE(104), VALUE(120)], column_at(grades_spreadsheet, 4))
    val _ = assertEquals_CellList([TEXT("Java HOF"), VALUE(100), VALUE(100), VALUE(84), VALUE(80), VALUE(120)], column_at(grades_spreadsheet, 5))
    val _ = assertEquals_CellList([TEXT("SML Binary Tree"), VALUE(100), VALUE(70), VALUE(85), VALUE(100), VALUE(130)], column_at(grades_spreadsheet, 6))
    val _ = assertEquals_CellList([TEXT("SML Pattern Matching"), VALUE(105), VALUE(65), VALUE(86), VALUE(105), VALUE(140)], column_at(grades_spreadsheet, 7))

    val _ = assertEquals_CellList( [TEXT("Name"), TEXT("Bobby Orr"), TEXT("Wayne Gretzky"), TEXT("Mario Lemieux")], column_at(hockey_spreadsheet, 0))
    val _ = assertEquals_CellList( [TEXT("Uniform Number"), VALUE(4), VALUE(99), VALUE(66)], column_at(hockey_spreadsheet, 1))
    val _ = assertEquals_CellList( [TEXT("Birth Year"), VALUE(1948), VALUE(1961), VALUE(1965)], column_at(hockey_spreadsheet, 2))
    val _ = assertEquals_CellList( [TEXT("Games Played"), VALUE(657), VALUE(1487), VALUE(915)], column_at(hockey_spreadsheet, 3))
    val _ = assertEquals_CellList( [TEXT("Goals"), VALUE(270), VALUE(894), VALUE(690)], column_at(hockey_spreadsheet, 4))
    val _ = assertEquals_CellList( [TEXT("Assists"), VALUE(645), VALUE(1963), VALUE(1033)], column_at(hockey_spreadsheet, 5))
    val _ = leave()

    val _ = enter("sum_values_in_row")

    val _ = assertEquals_Int(0, sum_values_in_row([[EMPTY]], 0))
    val _ = assertEquals_Int(0, sum_values_in_row([[TEXT("fred")]], 0))
    val _ = assertEquals_Int(425, sum_values_in_row([[VALUE(425)]], 0))
    val _ = assertEquals_Int(425, sum_values_in_row([[EMPTY, VALUE(425), TEXT("fred")]], 0))
    val _ = assertEquals_Int(656, sum_values_in_row([[VALUE(425), VALUE(231)]], 0))

    val _ = assertEquals_Int(10, sum_values_in_row(nums_spreadsheet, 0))
    val _ = assertEquals_Int(100, sum_values_in_row(nums_spreadsheet, 1))

    val _ = assertEquals_Int(0, sum_values_in_row(grades_spreadsheet, 0))
    val _ = assertEquals_Int(713, sum_values_in_row(grades_spreadsheet, 1))
    val _ = assertEquals_Int(575, sum_values_in_row(grades_spreadsheet, 2))
    val _ = assertEquals_Int(581, sum_values_in_row(grades_spreadsheet, 3))
    val _ = assertEquals_Int(668, sum_values_in_row(grades_spreadsheet, 4))
    val _ = assertEquals_Int(620, sum_values_in_row(grades_spreadsheet, 5))

    val _ = assertEquals_Int(0, sum_values_in_row(hockey_spreadsheet, 0))
    val _ = assertEquals_Int(3524, sum_values_in_row(hockey_spreadsheet, 1))
    val _ = assertEquals_Int(6404, sum_values_in_row(hockey_spreadsheet, 2))
    val _ = assertEquals_Int(4669, sum_values_in_row(hockey_spreadsheet, 3))
    val _ = leave()

    val _ = enter("sum_values_in_column")
    val _ = assertEquals_Int(11, sum_values_in_column(nums_spreadsheet, 0))
    val _ = assertEquals_Int(22, sum_values_in_column(nums_spreadsheet, 1))
    val _ = assertEquals_Int(33, sum_values_in_column(nums_spreadsheet, 2))
    val _ = assertEquals_Int(44, sum_values_in_column(nums_spreadsheet, 3))

    val _ = assertEquals_Int(0, sum_values_in_column(grades_spreadsheet, 0))
    val _ = assertEquals_Int(425, sum_values_in_column(grades_spreadsheet, 1))
    val _ = assertEquals_Int(414, sum_values_in_column(grades_spreadsheet, 2))
    val _ = assertEquals_Int(362, sum_values_in_column(grades_spreadsheet, 3))
    val _ = assertEquals_Int(486, sum_values_in_column(grades_spreadsheet, 4))
    val _ = assertEquals_Int(484, sum_values_in_column(grades_spreadsheet, 5))
    val _ = assertEquals_Int(485, sum_values_in_column(grades_spreadsheet, 6))
    val _ = assertEquals_Int(501, sum_values_in_column(grades_spreadsheet, 7))

    val _ = assertEquals_Int(0, sum_values_in_column(hockey_spreadsheet, 0))
    val _ = assertEquals_Int(169, sum_values_in_column(hockey_spreadsheet, 1))
    val _ = assertEquals_Int(5874, sum_values_in_column(hockey_spreadsheet, 2))
    val _ = assertEquals_Int(3059, sum_values_in_column(hockey_spreadsheet, 3))
    val _ = leave()

    val _ = enter("max_value_in_row")
    val _ = assertEquals_IntOption(NONE, max_value_in_row([[EMPTY]], 0))
    val _ = assertEquals_IntOption(NONE, max_value_in_row([[TEXT("fred")]], 0))
    val _ = assertEquals_IntOption(SOME(425), max_value_in_row([[VALUE(425)]], 0))
    val _ = assertEquals_IntOption(SOME(425), max_value_in_row([[EMPTY, VALUE(425), TEXT("fred")]], 0))
    val _ = assertEquals_IntOption(SOME(425), max_value_in_row([[VALUE(425), VALUE(231)]], 0))
    val _ = assertEquals_IntOption(SOME(425), max_value_in_row([[VALUE(231), VALUE(425)]], 0))

    val _ = assertEquals_IntOption(SOME(4), max_value_in_row(nums_spreadsheet, 0))
    val _ = assertEquals_IntOption(SOME(40), max_value_in_row(nums_spreadsheet, 1))

    val _ = assertEquals_IntOption(NONE, max_value_in_row(grades_spreadsheet, 0))
    val _ = assertEquals_IntOption(SOME(105), max_value_in_row(grades_spreadsheet, 1))
    val _ = assertEquals_IntOption(SOME(100), max_value_in_row(grades_spreadsheet, 2))
    val _ = assertEquals_IntOption(SOME(86), max_value_in_row(grades_spreadsheet, 3))
    val _ = assertEquals_IntOption(SOME(105), max_value_in_row(grades_spreadsheet, 4))
    val _ = assertEquals_IntOption(SOME(140), max_value_in_row(grades_spreadsheet, 5))

    val _ = assertEquals_IntOption(NONE, max_value_in_row(hockey_spreadsheet, 0))
    val _ = assertEquals_IntOption(SOME(1948), max_value_in_row(hockey_spreadsheet, 1))
    val _ = assertEquals_IntOption(SOME(1963), max_value_in_row(hockey_spreadsheet, 2))
    val _ = assertEquals_IntOption(SOME(1965), max_value_in_row(hockey_spreadsheet, 3))
    val _ = leave()

    val _ = enter("max_value_in_column")
    val _ = assertEquals_IntOption(SOME(10), max_value_in_column(nums_spreadsheet, 0))
    val _ = assertEquals_IntOption(SOME(20), max_value_in_column(nums_spreadsheet, 1))
    val _ = assertEquals_IntOption(SOME(30), max_value_in_column(nums_spreadsheet, 2))
    val _ = assertEquals_IntOption(SOME(40), max_value_in_column(nums_spreadsheet, 3))

    val _ = assertEquals_IntOption(NONE, max_value_in_column(grades_spreadsheet, 0))
    val _ = assertEquals_IntOption(SOME(100), max_value_in_column(grades_spreadsheet, 1))
    val _ = assertEquals_IntOption(SOME(104), max_value_in_column(grades_spreadsheet, 2))
    val _ = assertEquals_IntOption(SOME(100), max_value_in_column(grades_spreadsheet, 3))
    val _ = assertEquals_IntOption(SOME(120), max_value_in_column(grades_spreadsheet, 4))
    val _ = assertEquals_IntOption(SOME(120), max_value_in_column(grades_spreadsheet, 5))
    val _ = assertEquals_IntOption(SOME(130), max_value_in_column(grades_spreadsheet, 6))
    val _ = assertEquals_IntOption(SOME(140), max_value_in_column(grades_spreadsheet, 7))

    val _ = assertEquals_IntOption(NONE, max_value_in_column(hockey_spreadsheet, 0))
    val _ = assertEquals_IntOption(SOME(99), max_value_in_column(hockey_spreadsheet, 1))
    val _ = assertEquals_IntOption(SOME(1965), max_value_in_column(hockey_spreadsheet, 2))
    val _ = assertEquals_IntOption(SOME(1487), max_value_in_column(hockey_spreadsheet, 3))
    val _ = assertEquals_IntOption(SOME(894), max_value_in_column(hockey_spreadsheet, 4))
    val _ = assertEquals_IntOption(SOME(1963), max_value_in_column(hockey_spreadsheet, 5))
    val _ = leave()

    fun is_extra(cell) = 
        case cell of
             EMPTY => false
        |  TEXT(_) => false
        | VALUE(v) => v > 100

    fun is_even(cell) = 
        case cell of
             EMPTY => false
        |  TEXT(_) => false
        | VALUE(v) => (v mod 2) = 0

    val _ = enter("count_if_in_row")
    val _ = assertEquals_Int(2, count_if_in_row(nums_spreadsheet, 0, is_even))
    val _ = assertEquals_Int(4, count_if_in_row(nums_spreadsheet, 1, is_even))

    val _ = assertEquals_Int(0, count_if_in_row(grades_spreadsheet, 0, is_extra))
    val _ = assertEquals_Int(3, count_if_in_row(grades_spreadsheet, 1, is_extra))
    val _ = assertEquals_Int(0, count_if_in_row(grades_spreadsheet, 2, is_extra))
    val _ = assertEquals_Int(0, count_if_in_row(grades_spreadsheet, 3, is_extra))
    val _ = assertEquals_Int(3, count_if_in_row(grades_spreadsheet, 4, is_extra))
    val _ = assertEquals_Int(4, count_if_in_row(grades_spreadsheet, 5, is_extra))

    val _ = assertEquals_Int(0, count_if_in_row(hockey_spreadsheet, 0, is_even))
    val _ = assertEquals_Int(3, count_if_in_row(hockey_spreadsheet, 1, is_even))
    val _ = assertEquals_Int(1, count_if_in_row(hockey_spreadsheet, 2, is_even))
    val _ = assertEquals_Int(2, count_if_in_row(hockey_spreadsheet, 3, is_even))
    val _ = leave()

    val _ = enter("count_if_in_column")
    val _ = assertEquals_Int(1, count_if_in_column(nums_spreadsheet, 0, is_even))
    val _ = assertEquals_Int(2, count_if_in_column(nums_spreadsheet, 1, is_even))
    val _ = assertEquals_Int(1, count_if_in_column(nums_spreadsheet, 2, is_even))
    val _ = assertEquals_Int(2, count_if_in_column(nums_spreadsheet, 3, is_even))

    val _ = assertEquals_Int(0, count_if_in_column(grades_spreadsheet, 0, is_extra))
    val _ = assertEquals_Int(0, count_if_in_column(grades_spreadsheet, 1, is_extra))
    val _ = assertEquals_Int(2, count_if_in_column(grades_spreadsheet, 2, is_extra))
    val _ = assertEquals_Int(0, count_if_in_column(grades_spreadsheet, 3, is_extra))
    val _ = assertEquals_Int(3, count_if_in_column(grades_spreadsheet, 4, is_extra))
    val _ = assertEquals_Int(1, count_if_in_column(grades_spreadsheet, 5, is_extra))
    val _ = assertEquals_Int(1, count_if_in_column(grades_spreadsheet, 6, is_extra))
    val _ = assertEquals_Int(3, count_if_in_column(grades_spreadsheet, 7, is_extra))

    val _ = assertEquals_Int(0, count_if_in_column(hockey_spreadsheet, 0, is_even))
    val _ = assertEquals_Int(2, count_if_in_column(hockey_spreadsheet, 1, is_even))
    val _ = assertEquals_Int(1, count_if_in_column(hockey_spreadsheet, 2, is_even))
    val _ = assertEquals_Int(0, count_if_in_column(hockey_spreadsheet, 3, is_even))
    val _ = assertEquals_Int(3, count_if_in_column(hockey_spreadsheet, 4, is_even))
    val _ = assertEquals_Int(0, count_if_in_column(hockey_spreadsheet, 5, is_even))


    fun count_if_is_greater_than_max(column_index : int) : int =
        let
            fun is_greater_than_max(max) = 
                fn(cell) => 
                    case cell of
                         EMPTY => false
                    |  TEXT(_) => false
                    | VALUE(v) => v > max

            fun value_of_max(column_index : int) : int = 
                case cell_at(grades_spreadsheet, 1, column_index) of
                         EMPTY => raise Fail("exprected value; actual EMPTY")
                    |  TEXT(s) => raise Fail("exprected value; actual TEXT(\"" ^ s ^ "\")" )
                    | VALUE(v) => v
        in
            count_if_in_column(grades_spreadsheet, column_index, is_greater_than_max(value_of_max(column_index)))
        end

    val _ = assertEquals_Int(0, count_if_is_greater_than_max(1))
    val _ = assertEquals_Int(0, count_if_is_greater_than_max(2))
    val _ = assertEquals_Int(0, count_if_is_greater_than_max(3))
    val _ = assertEquals_Int(1, count_if_is_greater_than_max(4))
    val _ = assertEquals_Int(1, count_if_is_greater_than_max(5))
    val _ = assertEquals_Int(1, count_if_is_greater_than_max(6))
    val _ = assertEquals_Int(1, count_if_is_greater_than_max(7))

    val _ = leave()

    val _ = enter("EXTRA_CREDIT_to_dictionaries_using_headers_as_keys")
    val nums_dicts = EXTRA_CREDIT_to_dictionaries_using_headers_as_keys(nums_spreadsheet)
    val nums_dict_a = hd(nums_dicts)
    val _ = assertEquals_CellOption(SOME(VALUE(10)), SingleListDictionary.get(nums_dict_a, VALUE(1)))
    val _ = assertEquals_CellOption(SOME(VALUE(20)), SingleListDictionary.get(nums_dict_a, VALUE(2)))
    val _ = assertEquals_CellOption(SOME(VALUE(30)), SingleListDictionary.get(nums_dict_a, VALUE(3)))
    val _ = assertEquals_CellOption(SOME(VALUE(40)), SingleListDictionary.get(nums_dict_a, VALUE(4)))
    val _ = assertEquals_CellOption(NONE, SingleListDictionary.get(nums_dict_a, TEXT("NOT A HEADER")))

    val grades_dicts = EXTRA_CREDIT_to_dictionaries_using_headers_as_keys(grades_spreadsheet)
    val grades_dict_a = hd(grades_dicts)

    val _ = assertEquals_CellOption(SOME(TEXT("Max")), SingleListDictionary.get(grades_dict_a, TEXT("Name")))
    val _ = assertEquals_CellOption(SOME(VALUE(100)), SingleListDictionary.get(grades_dict_a, TEXT("Java List")))
    val _ = assertEquals_CellOption(SOME(VALUE(104)), SingleListDictionary.get(grades_dict_a, TEXT("SML Calendar")))
    val _ = assertEquals_CellOption(SOME(VALUE(100)), SingleListDictionary.get(grades_dict_a, TEXT("SML Hearts")))
    val _ = assertEquals_CellOption(SOME(VALUE(104)), SingleListDictionary.get(grades_dict_a, TEXT("SML Card Game")))
    val _ = assertEquals_CellOption(SOME(VALUE(100)), SingleListDictionary.get(grades_dict_a, TEXT("Java HOF")))
    val _ = assertEquals_CellOption(SOME(VALUE(100)), SingleListDictionary.get(grades_dict_a, TEXT("SML Binary Tree")))
    val _ = assertEquals_CellOption(SOME(VALUE(105)), SingleListDictionary.get(grades_dict_a, TEXT("SML Pattern Matching")))
    val _ = assertEquals_CellOption(NONE, SingleListDictionary.get(grades_dict_a, TEXT("NOT A HEADER")))

    val grades_dict_b = hd(tl(grades_dicts))
    val _ = assertEquals_CellOption(SOME(TEXT("Joshua Bloch")), SingleListDictionary.get(grades_dict_b, TEXT("Name")))
    val _ = assertEquals_CellOption(SOME(VALUE(100)), SingleListDictionary.get(grades_dict_b, TEXT("Java List")))
    val _ = assertEquals_CellOption(SOME(VALUE(85)), SingleListDictionary.get(grades_dict_b, TEXT("SML Calendar")))
    val _ = assertEquals_CellOption(SOME(VALUE(80)), SingleListDictionary.get(grades_dict_b, TEXT("SML Hearts")))
    val _ = assertEquals_CellOption(SOME(VALUE(75)), SingleListDictionary.get(grades_dict_b, TEXT("SML Card Game")))
    val _ = assertEquals_CellOption(SOME(VALUE(100)), SingleListDictionary.get(grades_dict_b, TEXT("Java HOF")))
    val _ = assertEquals_CellOption(SOME(VALUE(70)), SingleListDictionary.get(grades_dict_b, TEXT("SML Binary Tree")))
    val _ = assertEquals_CellOption(SOME(VALUE(65)), SingleListDictionary.get(grades_dict_b, TEXT("SML Pattern Matching")))
    val _ = assertEquals_CellOption(NONE, SingleListDictionary.get(grades_dict_a, TEXT("NOT A HEADER")))

    (* NOTE: skipping testing c and d *)

    val grades_dict_e = hd(tl(tl(tl(tl(grades_dicts)))))
    val _ = assertEquals_CellOption(SOME(TEXT("Shannon O'Ganns")), SingleListDictionary.get(grades_dict_e, TEXT("Name")))
    val _ = assertEquals_CellOption(SOME(VALUE(70)), SingleListDictionary.get(grades_dict_e, TEXT("Java List")))
    val _ = assertEquals_CellOption(SOME(VALUE(40)), SingleListDictionary.get(grades_dict_e, TEXT("SML Calendar")))
    val _ = assertEquals_CellOption(SOME(VALUE(0)), SingleListDictionary.get(grades_dict_e, TEXT("SML Hearts")))
    val _ = assertEquals_CellOption(SOME(VALUE(120)), SingleListDictionary.get(grades_dict_e, TEXT("SML Card Game")))
    val _ = assertEquals_CellOption(SOME(VALUE(120)), SingleListDictionary.get(grades_dict_e, TEXT("Java HOF")))
    val _ = assertEquals_CellOption(SOME(VALUE(130)), SingleListDictionary.get(grades_dict_e, TEXT("SML Binary Tree")))
    val _ = assertEquals_CellOption(SOME(VALUE(140)), SingleListDictionary.get(grades_dict_e, TEXT("SML Pattern Matching")))
    val _ = assertEquals_CellOption(NONE, SingleListDictionary.get(grades_dict_a, TEXT("NOT A HEADER")))


    val hockey_dicts = EXTRA_CREDIT_to_dictionaries_using_headers_as_keys(hockey_spreadsheet)
    val hockey_dict_a = hd(hockey_dicts)

    val _ = assertEquals_CellOption(SOME(TEXT("Bobby Orr")), SingleListDictionary.get(hockey_dict_a, TEXT("Name")))
    val _ = assertEquals_CellOption(SOME(VALUE(4)), SingleListDictionary.get(hockey_dict_a, TEXT("Uniform Number")))
    val _ = assertEquals_CellOption(SOME(VALUE(1948)), SingleListDictionary.get(hockey_dict_a, TEXT("Birth Year")))
    val _ = assertEquals_CellOption(SOME(VALUE(657)), SingleListDictionary.get(hockey_dict_a, TEXT("Games Played")))
    val _ = assertEquals_CellOption(SOME(VALUE(270)), SingleListDictionary.get(hockey_dict_a, TEXT("Goals")))
    val _ = assertEquals_CellOption(SOME(VALUE(645)), SingleListDictionary.get(hockey_dict_a, TEXT("Assists")))
    val _ = assertEquals_CellOption(NONE, SingleListDictionary.get(hockey_dict_a, TEXT("NOT A HEADER")))

    val hockey_dict_b = hd(tl(hockey_dicts))

    val _ = assertEquals_CellOption(SOME(TEXT("Wayne Gretzky")), SingleListDictionary.get(hockey_dict_b, TEXT("Name")))
    val _ = assertEquals_CellOption(SOME(VALUE(99)), SingleListDictionary.get(hockey_dict_b, TEXT("Uniform Number")))
    val _ = assertEquals_CellOption(SOME(VALUE(1961)), SingleListDictionary.get(hockey_dict_b, TEXT("Birth Year")))
    val _ = assertEquals_CellOption(SOME(VALUE(1487)), SingleListDictionary.get(hockey_dict_b, TEXT("Games Played")))
    val _ = assertEquals_CellOption(SOME(VALUE(894)), SingleListDictionary.get(hockey_dict_b, TEXT("Goals")))
    val _ = assertEquals_CellOption(SOME(VALUE(1963)), SingleListDictionary.get(hockey_dict_b, TEXT("Assists")))
    val _ = assertEquals_CellOption(NONE, SingleListDictionary.get(hockey_dict_b, TEXT("NOT A HEADER")))

    val hockey_dict_c = hd(tl(tl(hockey_dicts)))

    val _ = assertEquals_CellOption(SOME(TEXT("Mario Lemieux")), SingleListDictionary.get(hockey_dict_c, TEXT("Name")))
    val _ = assertEquals_CellOption(SOME(VALUE(66)), SingleListDictionary.get(hockey_dict_c, TEXT("Uniform Number")))
    val _ = assertEquals_CellOption(SOME(VALUE(1965)), SingleListDictionary.get(hockey_dict_c, TEXT("Birth Year")))
    val _ = assertEquals_CellOption(SOME(VALUE(915)), SingleListDictionary.get(hockey_dict_c, TEXT("Games Played")))
    val _ = assertEquals_CellOption(SOME(VALUE(690)), SingleListDictionary.get(hockey_dict_c, TEXT("Goals")))
    val _ = assertEquals_CellOption(SOME(VALUE(1033)), SingleListDictionary.get(hockey_dict_c, TEXT("Assists")))
    val _ = assertEquals_CellOption(NONE, SingleListDictionary.get(hockey_dict_c, TEXT("NOT A HEADER")))

    val _ = leave()

    val _ = print "<<< end test error output\n\n"

in 
    () 
end


