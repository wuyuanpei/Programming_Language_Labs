fun is_separator(c : char) : bool =
        c = #"/"

val split_path = String.fields is_separator "git/425/sml/spreadsheet"