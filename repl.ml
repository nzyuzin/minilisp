exception EmptyCons
exception NotSexp
exception NotMatchingBraces

type 'a cons = Nil | Cons of 'a * 'a cons
and sexp = Value of string | Sexp of sexp cons

let rec string_of_cons c to_string = match c with
  | Nil -> "()"
  | Cons(a, rest) -> "(" ^ to_string a ^ " " ^ string_of_cons rest to_string ^ ")"

let rec string_of_sexp c =
  let rec string_of_cons_content c = match c with
    | Nil -> ""
    | Cons(a, Nil) -> string_of_sexp a
    | Cons(a, rest) -> string_of_sexp a ^ " " ^ string_of_cons_content rest in
  match c with
  | Value(str) -> str
  | Sexp(cons) -> "(" ^ string_of_cons_content cons ^ ")"

let car = function
  | Nil -> raise EmptyCons
  | Cons(a, b) -> a

let cdr = function
  | Nil -> raise EmptyCons
  | Cons(a, b) -> b

let rec read_word str =
  let length = String.length str in
  let rec inner pos braces =
    if pos = length then
      if braces = 0 then ""
      else raise NotMatchingBraces
    else
      let symbol = String.sub str pos 1 in
      if symbol = " " && braces = 0 then ""
      else if symbol = "(" then
        symbol ^ inner (pos + 1) (braces + 1)
      else if symbol = ")" then
        symbol ^ inner (pos + 1) (braces - 1)
      else
        symbol ^ inner (pos + 1) braces in
  inner 0 0

let rec parse_input input: sexp =
  let rec trim_head str =
    let length = String.length str in
    if length = 0 then str
    else
      if str.[0] = ' ' then trim_head (String.sub str 1 (length - 1))
      else str in
  let rec parse_cons str: sexp cons =
    let length = String.length str in
    if length = 0 || str = "()"
    then
      Nil
    else
      let word = read_word str in
      let word_length = String.length word in
      let after_word = String.sub str word_length (length - word_length) in
      Cons(parse_sexp word, parse_cons (String.trim (trim_head after_word)))
  and parse_sexp str: sexp =
    let length = String.length str in
    let rec no_spaces s =
      let s_length = String.length s in
      if s_length = 0 then
        true
      else if s.[0] = ' ' then false
      else no_spaces (String.sub s 1 (s_length - 1)) in
    if length = 0 then Sexp(Nil)
    else if str.[0] = '(' && str.[length - 1] = ')' then
      Sexp(parse_cons (String.trim (trim_head (String.sub str 1 (length - 2)))))
    else if no_spaces str then
      Value(read_word str)
    else
      raise NotSexp in
  parse_sexp (String.trim input)

let rec repl () = begin
  let error str = print_endline ("Error: " ^ str) in
  let print_caret () = print_string "/> " in
  let read_line () = input_line stdin in
  let process_input () =
    try
      let input = read_line () in
      print_endline (string_of_sexp (parse_input input));
    with
      | EmptyCons -> error "Empty cons!"
      | NotSexp -> error "Not an S-expression!"
      | NotMatchingBraces -> error "Not matching amount of braces!" in
  print_caret ();
  flush stdout;
  process_input ();
  print_endline "";
  repl ()
end

let _ =
  repl ()
