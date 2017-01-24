type ltype =
  | LUnit
  | LInt of int
  | LBool of bool
  | LString of string
  | LCons of ltype * ltype
  | LIdentifier of string
  | LFunction of (ltype -> context -> ltype)
and context = (string * ltype) list ref

exception TypeError of ltype * string

let rec string_of_ltype =
  let rec nil_terminated = function
    | LCons(a, LUnit) -> true
    | LCons(_, rest) -> nil_terminated rest
    | _ -> false in
  let rec string_of_lcons = function
    | LCons(a, LUnit) -> string_of_ltype a
    | LCons(a, rest) when nil_terminated rest -> string_of_ltype a ^ " " ^ string_of_lcons rest
    | LCons(a, rest) -> string_of_ltype a ^ " . " ^ string_of_ltype rest
    | x -> string_of_ltype x in
  function
  | LUnit -> "()"
  | LInt(i) -> string_of_int i
  | LBool(b) -> if b then "#t" else "#f"
  | LString(s) -> s
  | LCons(c, d) as x -> "(" ^ string_of_lcons x ^ ")"
  | LIdentifier(s) -> s
  | LFunction(_) -> "<function>"

let inside_char_bounds chr f s =
  let code = Char.code chr in
  code >= (Char.code f) && code <= (Char.code s)

let is_digit chr =
  inside_char_bounds chr '0' '9'

let is_alpha chr =
  (inside_char_bounds chr 'a' 'z') || (inside_char_bounds chr 'A' 'Z')

let is_alphanum chr =
  is_digit chr || is_alpha chr

let after_first_char str =
  String.sub str 1 (String.length str - 1)

let rec is_int (str: string) =
  let rec is_digits s =
    let length = String.length s in
    if length = 0 then false
    else if length = 1 then is_digit s.[0]
    else is_digit s.[0] && is_digits (after_first_char s) in
  if str.[0] = '-' && String.length str > 1 then
    is_digits (after_first_char str)
  else
    is_digits str

let is_bool (x: string) =
  x = "#t" || x = "#f"

let is_string (x: string) =
  let length = String.length x in
  x.[0] = '"' && x.[length - 1] = '"'

let is_identifier str: bool =
  let extended_alphabetic_characters = "!$%&*+-./:<=>?@^_~" in
  let is_allowed_symbol chr =
    is_alphanum chr || String.contains extended_alphabetic_characters chr in
  let rec contains_only_allowed s =
    if String.length s = 0 then false
    else if String.length s = 1 then is_allowed_symbol s.[0]
    else is_allowed_symbol s.[0] && contains_only_allowed (after_first_char s) in
  let is_allowed_first_char chr =
    (not (is_digit chr)) && is_allowed_symbol chr in
  let length = String.length str in
  if length = 0 then false
  else if length = 1 then is_allowed_first_char str.[0]
  else is_allowed_first_char str.[0] && contains_only_allowed(after_first_char str)

let rec ltype_is_list = function
  | LUnit -> true
  | LCons(_, xs) -> ltype_is_list xs
  | _ -> false

let rec ltype_map l f =
  match l with
  | LUnit -> LUnit
  | LCons(x, xs) -> LCons(f(x), ltype_map xs f)
  | x -> raise (TypeError (x, "list"))

let ltype_car = function
  | LCons(x, _) -> x
  | x -> raise (TypeError (x, "cons"))

let ltype_cdr = function
  | LCons(_, xs) -> xs
  | x -> raise (TypeError (x, "cons"))

let rec ltype_length = function
  | LUnit -> 0
  | LCons(_, xs) -> 1 + ltype_length xs
  | x -> raise (TypeError (x, "list"))
