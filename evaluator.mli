exception IllFormedSpecialForm of string
exception CannotEvaluate
exception NotApplicable of string
exception UnboundValue of string
exception ArgumentsMismatch of int * int

val eval: Ltype.ltype -> Ltype.context -> Ltype.ltype

val apply: Ltype.ltype -> Ltype.ltype -> Ltype.context -> Ltype.ltype
