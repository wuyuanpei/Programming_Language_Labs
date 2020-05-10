(* SML comes with a length function *)
(* http://sml-family.org/Basis/list.html#SIG:LIST.length:VAL *)
val length = "shadow built in length method to prevent mistakenly calling it"

exception NotYetImplemented

(* Richard Wu *)
(* define a function my_length which returns the length of the specified list *)
fun my_length(values) =
	if values = nil then
		0
	else
		1 + my_length(tl values)
