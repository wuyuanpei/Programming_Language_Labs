(* Dennis Cosgrove *)
use "../unit_test/unit_test.sml";
use "binary_tree.sml";

signature CORE_UNIT_TEST_BINARY_TREE = sig
    val assertInsertAll_Int : int list -> unit
    val assertInsertAll_String : string list -> unit

    val assertInsertAllInRandomOrderRepeatedly_Int : (int * int list) -> unit
    val assertInsertAllInRandomOrderRepeatedly_String : (int * string list) -> unit
    
    val assertInsertAllInOrderFollowedByRemove_Int : (int list * int) -> unit
    val assertInsertAllInOrderFollowedByRemove_String : (string list * string) -> unit
    
    val assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrderRepeatedly_Int : (int * int list) -> unit
    val assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrderRepeatedly_String : (int * string list) -> unit
end

structure CoreUnitTestBinaryTree :> CORE_UNIT_TEST_BINARY_TREE = struct
    open BinaryTree
    fun list_to_string(item_to_repr) = (Repr.toString o Repr.listToRepr item_to_repr)

    fun insertAll compare_function (xs : 'a list) : ('a tree) =
        let 
            fun helper(xs) =
                case xs of
                        [] => LEAF
                | x :: xs' => insert compare_function (helper(xs')) x
        in
            helper(xs)
        end

    fun treeToList(t : 'a tree) : 'a list =
        case t of
                          LEAF => []
        | BRANCH(left,v,right) => (treeToList(left))@(v::(treeToList(right)))

    fun assertInsertAll item_to_repr compare_function (original_list: ''a list) : unit = 
        let
            val expected_list = (ListMergeSort.uniqueSort compare_function original_list)
            val actual_tree = (insertAll compare_function original_list)
            val actual_list = treeToList(actual_tree)

            val item_list_to_string = list_to_string(item_to_repr)
            val expected_list_string = item_list_to_string(expected_list)
        in
            if expected_list = actual_list
            then UnitTest.success("equals: " ^ expected_list_string)
            else 
                let
                    val original_list_string = item_list_to_string(original_list)
                    val item_to_string = (Repr.toString o item_to_repr)
                    val actual_tree_string = BinaryTree.to_string item_to_string actual_tree
                    val actual_list_string = item_list_to_string(actual_list)
                in
                    UnitTest.failure("expected: " ^ expected_list_string ^ "; actual: " ^ actual_list_string ^ "\n!!!                    original argument: " ^ original_list_string ^ "\n!!!                    tree: " ^ actual_tree_string)
                end
        end

    val assertInsertAll_Int = assertInsertAll Repr.I Int.compare
    val assertInsertAll_String = assertInsertAll Repr.QUOTED_STRING String.compare

    fun remove_random compare_function (xs : ''a list, r : Random.rand) : (''a*(''a list)) = 
        let 
            val i = Random.randInt(r) mod List.length(xs)
            val x = List.nth(xs, i)
            val xs' = List.filter (fn v=> compare_function(v,x) <> EQUAL) xs
        in
            (x, xs')
        end

    fun shuffle compare_function (original_list : ''a list, r : Random.rand) : ''a list =
        let
            val input = ref original_list
            val output = ref []
            val _ = 
                while List.length(!input) > 0 do
                    let
                        val (v, input') = remove_random compare_function (!input, r)
                        val _ = output := (v :: !output)
                        val _ = input := input'
                    in
                        ()
                    end
        in
            !output
        end


    fun assertInsertAllInRandomOrder item_to_repr compare_function (original_list: ''a list, r : Random.rand) : unit = 
        let
            val input = ref original_list
            val output = ref []
        in
            while List.length(!input) > 0 do
                let
                    val (v, input') = remove_random compare_function (!input, r)
                    val _ = output := (v :: !output)
                    val _ = input := input'
                    val xs = !output
                in
                    assertInsertAll item_to_repr compare_function xs
                end 
        end

    fun assertInsertAllInRandomOrderRepeatedly item_to_repr compare_function (n : int, original_list: ''a list) : unit = 
        let
            val r = Random.rand(425, 231)
            val i = ref(0)
        in
            while !i < n do 
                let
                    val _ = print("\n    =========\n    iteration: "^ Int.toString(!i) ^ "; assertInsertAllInRandomOrderRepeatedly(" ^ Int.toString(n) ^ ", " ^ list_to_string(item_to_repr)(original_list) ^ ")\n    =========\n")
                    val _ = i := !i + 1
                in
                    assertInsertAllInRandomOrder item_to_repr compare_function (original_list, r)
                end
        end

    val assertInsertAllInRandomOrderRepeatedly_Int = assertInsertAllInRandomOrderRepeatedly Repr.I Int.compare
    val assertInsertAllInRandomOrderRepeatedly_String = assertInsertAllInRandomOrderRepeatedly Repr.QUOTED_STRING String.compare

    fun assertInsertAllInOrderFollowedByRemove item_to_repr compare_function (values: ''a list, value_to_remove : ''a) : unit = 
        let
            val original_tree = (insertAll compare_function values)
            fun equal_less_greater(v) =
                compare_function(value_to_remove,v)
            val actual_tree_post_remove = BinaryTree.remove equal_less_greater original_tree
            val actual_values_post_remove = treeToList(actual_tree_post_remove)

            val values_post_remove = List.filter (fn v=> equal_less_greater(v) <> EQUAL) values
            val expected_values_post_remove = (ListMergeSort.uniqueSort compare_function values_post_remove)

            val item_list_to_string = list_to_string(item_to_repr)
        in
            if expected_values_post_remove = actual_values_post_remove
            then UnitTest.success("equals: " ^ item_list_to_string(expected_values_post_remove))
            else 
                let
                    val item_to_string = (Repr.toString o item_to_repr)
                    val original_tree_string = BinaryTree.to_string item_to_string original_tree
                    val actual_tree_string = BinaryTree.to_string item_to_string actual_tree_post_remove
                in
                    UnitTest.failure("expected: " ^ item_list_to_string(expected_values_post_remove) ^ "; actual: " ^ item_list_to_string(actual_values_post_remove) ^ "\n!!!                    assertInsertAllInOrderFollowedByRemove(" ^ item_list_to_string(values) ^ ", " ^ item_to_string(value_to_remove) ^ ")\n!!!                    original tree: " ^ original_tree_string ^ "\n!!!                    post remove tree: " ^ actual_tree_string)
                end
        end

    val assertInsertAllInOrderFollowedByRemove_Int = assertInsertAllInOrderFollowedByRemove Repr.I Int.compare
    val assertInsertAllInOrderFollowedByRemove_String = assertInsertAllInOrderFollowedByRemove Repr.QUOTED_STRING String.compare

    fun assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrder item_to_repr compare_function (original_list: ''a list, r : Random.rand) : unit = 
        let
            val shuffled_list = shuffle compare_function (original_list, r)
            val input = ref shuffled_list
            val output : ''a list ref = ref []
        in
            while List.length(!input) > 0 do
                let
                    val xs = !input
                    val (v, input') = remove_random compare_function (!input, r)
                    val _ = output := (v :: !output)
                    val _ = input := input'
                in
                    assertInsertAllInOrderFollowedByRemove item_to_repr compare_function (xs, v)
                end 
        end

    fun assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrderRepeatedly item_to_repr compare_function (n : int, original_list: ''a list) : unit = 
        let
            val r = Random.rand(425, 231)
            val i = ref(0)
        in
            while !i < n do 
                let
                    val _ = print("\n    =========\n    iteration: "^ Int.toString(!i) ^ "; assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrderRepeatedly(" ^ Int.toString(n) ^ ", " ^ list_to_string(item_to_repr)(original_list) ^ ")\n    =========\n")
                    val _ = i := !i + 1
                in
                    assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrder item_to_repr compare_function (original_list, r)
                end
        end

    val assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrderRepeatedly_Int = assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrderRepeatedly Repr.I Int.compare
    val assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrderRepeatedly_String = assertInsertAllInRandomOrderFollowedByRemoveEachInRandomOrderRepeatedly Repr.QUOTED_STRING String.compare
end