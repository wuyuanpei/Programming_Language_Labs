signature COMMAND_LINE_ARGS = sig
    val getBoolOrDefault : (string*bool) -> bool
    val getStringOrDefault : (string*string) -> string
    val getStringOption : (string) -> string option
end

structure CommandLineArgs :> COMMAND_LINE_ARGS = struct
    fun getOptionValueText (argName:string) : string option = 
        let 
            val args = CommandLine.arguments()
            val startsWithDashDashOptionEquals = String.isPrefix ("--"^argName^"=")
            val optionText = List.find startsWithDashDashOptionEquals args
        in
            case optionText of
                 NONE => NONE
            | SOME(s) => SOME(String.extract(s, String.size(argName)+3, NONE))
        end

    fun getOption fromString (argName:string) : 'a option =
        case(getOptionValueText(argName)) of
             NONE => NONE
        | SOME(s) => fromString(s)

    fun getOrDefault fromString (argName:string, defaultValue:'a) : 'a =
        case(getOptionValueText(argName)) of
             NONE => defaultValue
        | SOME(s) => case fromString(s) of
                          NONE => raise Fail(s)
                     | SOME(b) => b

    fun stringOptionFromString(s) =
        SOME(s)
    
    val getBoolOrDefault = getOrDefault Bool.fromString
    val getStringOrDefault = getOrDefault stringOptionFromString
    val getStringOption = getOption stringOptionFromString
end
