exception EmptyCons
exception NotSexp
exception NotMatchingBraces
exception NotApplicable
exception CannotEvaluate
exception UnboundValue of string
exception CannotCast of string * string

type 'a cons = Nil | Cons of 'a * 'a cons
and 'b sexp = Value of 'b | Sexp of 'b sexp cons

let rec string_of_cons c to_string = match c with
  | Nil -> "()"
  | Cons(a, rest) -> "(" ^ to_string a ^ " " ^ string_of_cons rest to_string ^ ")"

let rec string_of_sexp c to_string =
  let rec string_of_cons_content c = match c with
    | Nil -> ""
    | Cons(a, Nil) -> string_of_sexp a to_string
    | Cons(a, rest) -> string_of_sexp a to_string ^ " " ^ string_of_cons_content rest in
  match c with
  | Value(v) -> to_string v
  | Sexp(cons) -> "(" ^ string_of_cons_content cons ^ ")"

type lang_type =
  | Unit
  | Int of int
  | String of string
  | Variable of lang_type
  | Function of (lang_type cons -> lang_type)

let rec string_of_lang_type = function
  | Unit -> "()"
  | Int(i) -> string_of_int i
  | String(s) -> s
  | Variable(lt) -> string_of_lang_type lt
  | Function(_) -> "<function>"

let rec is_int (x: string) =
  let is_digit c =
    let code = Char.code c in
    code > (Char.code '0') && code < (Char.code '9') in
  let length = String.length x in
  if length = 0 then false
  else if length = 1 then is_digit x.[0]
  else is_digit x.[0] && is_int (String.sub x 1 (length - 1))

let int_of_lang_type = function
  | Int(i) -> i
  | v -> raise (CannotCast (string_of_lang_type v, "int"))

let is_string (x: string) =
  let length = String.length x in
  (x.[0] = '\'' && x.[length - 1] = '\'')
  || (x.[0] = '"' && x.[length - 1] = '"')

let rec lookup (name: string) (context: (string * lang_type) list): lang_type option =
  match context with
  | [] -> None
  | (name', value) :: xs -> if name = name' then (Some value) else lookup name xs

let is_variable (x: string) context = match lookup x context with
| Some(Variable(_)) -> true
| _ -> false

let is_function (x: string) context = match lookup x context with
| Some(Function(_)) -> true
| _ -> false

let rec cons_map (f: 'a -> 'b) (l: 'a cons): 'b cons = match l with
  | Nil -> Nil
  | Cons(v, r) -> Cons(f(v), cons_map f r)

let rec eval (expression: string sexp) context: lang_type =
  let unwrap = function
    | Some x -> x
    | None -> raise (Failure "Unwrap on eval failed") in
  match expression with
  | Value(v) ->
      if is_int v then Int(int_of_string v)
      else if is_string v then String(v)
      else if is_variable v context then unwrap (lookup v context)
      else if is_function v context then unwrap (lookup v context)
      else raise (UnboundValue v)
  | Sexp(c) -> begin  match c with
    | Nil -> Unit
    | Cons(v, rest) -> apply v rest context
  end

and apply (f: string sexp) (arguments: string sexp cons) context: lang_type =
  let evaled_func = eval f context in
  let evaled_arguments: lang_type cons = cons_map (fun x -> eval x context) arguments in
  match evaled_func with
  | Function(func) -> func evaled_arguments
  | _ -> raise NotApplicable

let cons a b = Cons(a, b)

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

let rec parse_input input: string sexp =
  let rec trim_head str =
    let length = String.length str in
    if length = 0 then str
    else
      if str.[0] = ' ' then trim_head (String.sub str 1 (length - 1))
      else str in
  let rec parse_cons str: string sexp cons =
    let length = String.length str in
    if length = 0 || str = "()"
    then
      Nil
    else
      let word = read_word str in
      let word_length = String.length word in
      let after_word = String.sub str word_length (length - word_length) in
      Cons(parse_sexp word, parse_cons (String.trim (trim_head after_word)))
  and parse_sexp str: string sexp =
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


let global_context: (string * lang_type) list =
  [("+", Function((fun c -> Int(int_of_lang_type(car(c)) + int_of_lang_type(car(cdr(c)))))))]

let rec repl () = begin
  let error str = print_endline ("Error: " ^ str) in
  let print_caret () = print_string "/> " in
  let read_line () = input_line stdin in
  let process_input () =
    try
      let input = read_line () in
      let parsed_input = parse_input input in
      let evaled_input = Value(eval parsed_input global_context) in
      print_endline (string_of_sexp evaled_input string_of_lang_type);
    with
      | EmptyCons -> error "Empty cons!"
      | NotSexp -> error "Not an S-expression!"
      | NotMatchingBraces -> error "Not matching amount of braces!"
      | NotApplicable -> error "Not applicable operation!"
      | CannotEvaluate -> error "Expression cannot be evaluated!"
      | CannotCast(value, to_type) -> error ("Cannot cast " ^ value ^ "to type " ^ to_type ^ "!")
      | UnboundValue(s) -> error ("Unbound value: " ^ s ^ "!")
      | End_of_file ->
          print_endline "";
          print_endline "Terminated by user";
          exit 0 in
  print_caret ();
  flush stdout;
  process_input ();
  print_endline "";
  repl ()
end

let _ =
  repl ()
