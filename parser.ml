open Ltype

exception NotSexp of string
exception NotMatchingBraces
exception MalformedString of string
exception InvalidIdentifier of string

type 'b sexp = Value of 'b | Sexp of 'b sexp list

let rec ltype_of_sexp (expression: string sexp): ltype =
  let bool_of_ltype_string b =
    if b = "#t" then true
    else if b = "#f" then false
    else raise (Failure (b ^ " is not bool")) in
  let rec ltype_of_sexp_list = function
    | [] -> LUnit
    | x :: xs -> LCons(ltype_of_sexp x, ltype_of_sexp_list xs) in
  match expression with
  | Value(v) ->
      if is_int v then LInt(int_of_string v)
      else if is_bool v then LBool(bool_of_ltype_string v)
      else if is_string v then LString(String.sub v 1 (String.length v - 2))
      else if is_identifier v then LIdentifier(String.lowercase_ascii v)
      else raise (InvalidIdentifier v)
  | Sexp(c) -> begin match c with
    | [] -> LUnit
    | x :: xs -> LCons(ltype_of_sexp x, ltype_of_sexp_list xs)
  end

let remove_comments str =
  let com_pos = try String.index str ';' with Not_found -> -1 in
  if com_pos != -1 then
    String.sub str 0 com_pos
  else
    str

let balanced_braces s =
  let rec inner pos balance =
    if pos = String.length s then balance
    else if s.[pos] = '(' then inner (pos + 1) (balance + 1)
    else if s.[pos] = ')' then inner (pos + 1) (balance - 1)
    else inner (pos + 1) balance in
  inner 0 0 = 0

let parse_input (source: string): ltype =
  let rec surround_braces str =
    let surround_with_spaces s =
      " " ^ s ^ " " in
    if String.length str = 0 then
      str
    else
      let first_symbol = String.sub str 0 1 in
      if first_symbol = "(" || first_symbol = ")" then
        (surround_with_spaces first_symbol) ^ surround_braces (after_first_char str)
      else
        first_symbol ^ surround_braces (after_first_char str) in
  let read_word (s: string): string =
    let rec read_str str acc =
      let length = String.length str in
      if length = 0 then
        raise (MalformedString acc)
      else
        let f = String.sub str 0 1 in
        if f = "\"" then (acc ^ f)
        else read_str (after_first_char str) (acc ^ f) in
    let rec inner str braces =
      let length = String.length str in
      if length = 0 then
        if braces = 0 then ""
        else raise NotMatchingBraces
      else
        let first_symbol = String.sub str 0 1 in
        let rest = after_first_char str in
        if first_symbol = "\"" then
          let str_literal = read_str rest first_symbol in
          let str_literal_length = String.length str_literal in
          let after_literal = String.sub str str_literal_length (length - str_literal_length) in
          str_literal ^ inner after_literal braces
        else if first_symbol = " " && braces = 0 then ""
        else if first_symbol = "(" then
          first_symbol ^ inner rest (braces + 1)
        else if first_symbol = ")" then
          first_symbol ^ inner rest (braces - 1)
        else
          first_symbol ^ inner rest braces in
    inner s 0 in
  let trim str =
    String.trim str in
  let rec parse_cons str: string sexp list =
    let length = String.length str in
    if length = 0 then []
    else if str = "()" then [Sexp([])]
    else
      let word = read_word str in
      let word_length = String.length word in
      let after_word = String.sub str word_length (length - word_length) in
      parse_sexp word :: parse_cons (trim after_word)
  and parse_sexp (str: string): string sexp =
    let length = String.length str in
    let rec no_spaces s =
      let s_length = String.length s in
      if s_length = 0 then
        true
      else if s.[0] = ' ' then false
      else no_spaces (after_first_char s) in
    let is_literal s =
      (no_spaces s) || (s.[0] = '"' && s.[String.length s - 1] = '"') in
    if length = 0 then Sexp([])
    else if str.[0] = '(' && str.[length - 1] = ')' && balanced_braces str then
      Sexp(parse_cons (trim (String.sub str 1 (length - 2))))
    else if is_literal str then
      Value(read_word str)
    else
      raise (NotSexp str) in
  let prepared_input = trim (surround_braces (remove_comments source)) in
  ltype_of_sexp (parse_sexp (read_word prepared_input))

