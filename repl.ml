exception NotSexp
exception NotMatchingBraces
exception NotApplicable
exception CannotEvaluate
exception UnboundValue of string
exception CannotCast of string * string
exception ArgumentsMismatch of int * int
exception TypeError

type 'b sexp = Value of 'b | Sexp of 'b sexp list

let rec string_of_sexp c to_string =
  let rec string_of_list_content c = match c with
    | [] -> ""
    | [a] -> string_of_sexp a to_string
    | a :: rest -> string_of_sexp a to_string ^ " " ^ string_of_list_content rest in
  match c with
  | Value(v) -> to_string v
  | Sexp(cons) -> "(" ^ string_of_list_content cons ^ ")"

type ltype =
  | LUnit
  | LInt of int
  | LString of string
  | LCons of ltype * ltype
  | LVariable of ltype
  | LFunction of (ltype list -> ltype)

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
  | LString(s) -> s
  | LCons(c, d) as x -> "(" ^ string_of_lcons x ^ ")"
  | LVariable(lt) -> string_of_ltype lt
  | LFunction(_) -> "<function>"

let rec lcons_of_list = function
  | [] -> LUnit
  | x :: [] -> x
  | x :: xs -> LCons(x, lcons_of_list xs)

let rec is_int (x: string) =
  let is_digit c =
    let code = Char.code c in
    code >= (Char.code '0') && code <= (Char.code '9') in
  let length = String.length x in
  if length = 0 then false
  else if length = 1 then is_digit x.[0]
  else is_digit x.[0] && is_int (String.sub x 1 (length - 1))

let int_of_ltype = function
  | LInt(i) -> i
  | v -> raise (CannotCast (string_of_ltype v, "int"))

let is_string (x: string) =
  let length = String.length x in
  (x.[0] = '\'' && x.[length - 1] = '\'')
  || (x.[0] = '"' && x.[length - 1] = '"')

let rec lookup (name: string) (context: (string * ltype) list): ltype option =
  match context with
  | [] -> None
  | (name', value) :: xs -> if name = name' then (Some value) else lookup name xs

let is_variable (x: string) context = match lookup x context with
| Some(LVariable(_)) -> true
| _ -> false

let is_function (x: string) context = match lookup x context with
| Some(LFunction(_)) -> true
| _ -> false

let rec eval (expression: string sexp) context: ltype =
  let unwrap = function
    | Some x -> x
    | None -> raise (Failure "Unwrap on eval failed") in
  match expression with
  | Value(v) ->
      if is_int v then LInt(int_of_string v)
      else if is_string v then LString(v)
      else if is_variable v context then unwrap (lookup v context)
      else if is_function v context then unwrap (lookup v context)
      else raise (UnboundValue v)
  | Sexp(c) -> begin  match c with
    | [] -> LUnit
    | v :: rest -> apply v rest context
  end

and apply (f: string sexp) (arguments: string sexp list) context: ltype =
  let evaled_func = eval f context in
  let evaled_arguments: ltype list = List.map (fun x -> eval x context) arguments in
  match evaled_func with
  | LFunction(func) -> func evaled_arguments
  | _ -> raise NotApplicable

let rec parse_input (source: unit -> string): string sexp =
  let read_word (s: string): string =
    let rec inner str braces =
      let length = String.length str in
      if length = 0 then
        if braces = 0 then ""
        else if braces > 0 then inner (" " ^ source ()) braces
        else raise NotMatchingBraces
      else
        let first_symbol = String.sub str 0 1 in
        let rest = String.sub str 1 (length - 1) in
        if first_symbol = " " && braces = 0 then ""
        else if first_symbol = "(" then
          first_symbol ^ inner rest (braces + 1)
        else if first_symbol = ")" then
          first_symbol ^ inner rest (braces - 1)
        else
          first_symbol ^ inner rest braces in
    inner s 0 in
  let trim str =
    let rec trim_head str =
      let length = String.length str in
      if length = 0 then str
      else
        if str.[0] = ' ' then trim_head (String.sub str 1 (length - 1))
        else str in
    String.trim (trim_head str) in
  let rec parse_cons str: string sexp list =
    let length = String.length str in
    if length = 0 then []
    else if str = "()" then [Sexp([])]
    else
      let word = read_word str in
      let word_length = String.length word in
      let after_word = String.sub str word_length (length - word_length) in
      parse_sexp word :: parse_cons (trim after_word)
  and parse_sexp str: string sexp =
    let length = String.length str in
    let rec no_spaces s =
      let s_length = String.length s in
      if s_length = 0 then
        true
      else if s.[0] = ' ' then false
      else no_spaces (String.sub s 1 (s_length - 1)) in
    if length = 0 then Sexp([])
    else if str.[0] = '(' && str.[length - 1] = ')' then
      Sexp(parse_cons (trim (String.sub str 1 (length - 2))))
    else if no_spaces str then
      Value(read_word str)
    else
      raise NotSexp in
  parse_sexp (read_word (trim (source ())))

let check_arguments expected actual =
  raise (ArgumentsMismatch(expected, actual))

let global_context: (string * ltype) list =
  [("+", LFunction(fun arguments ->
     LInt(int_of_ltype(List.hd arguments) + int_of_ltype(List.hd (List.tl arguments)))));
   ("cons", LFunction(fun arguments ->
     let length = List.length arguments in
     if length = 2 then
       let f = List.hd arguments in
       let s = lcons_of_list (List.tl arguments) in
       LCons(f, s)
     else check_arguments 2  length));
   ("car", LFunction(fun arguments ->
     let length = List.length arguments in
     if length = 1 then
       match List.hd arguments with
       | LCons(f, s) -> f
       | _ -> raise TypeError
     else check_arguments 1 length));
   ("cdr", LFunction(fun arguments ->
     let length = List.length arguments in
     if length = 1 then
       match List.hd arguments with
       | LCons(f, s) -> s
       | _ -> raise TypeError
     else check_arguments 1 length))]

let rec repl () = begin
  let error str = print_endline ("Error: " ^ str) in
  let print_caret () = print_string "/> " in
  let read_line () = input_line stdin in
  let process_input (): string sexp option =
    let parsed_input = ref None in
    let try_read =
      try
        parsed_input := Some (parse_input read_line)
      with
        | NotSexp -> error "Not an S-expression!"
        | NotMatchingBraces -> error "Not matching amount of braces!"
        | End_of_file -> begin
            print_endline "";
            print_endline "End of input stream reached.";
            exit 0;
          end in
    try_read;
    !parsed_input in
  print_caret ();
  flush stdout;
  begin
    match process_input () with
      | None -> ();
      | Some parsed_sexp ->
        try
          let evaled_input = eval parsed_sexp global_context in
          print_endline (";Value: " ^ (string_of_ltype evaled_input))
        with
          | NotApplicable -> error "Not applicable operation!"
          | CannotEvaluate -> error "Expression cannot be evaluated!"
          | CannotCast(value, to_type) ->
              error ("Cannot cast " ^ value ^ " to type " ^ to_type ^ "!")
          | UnboundValue(s) -> error ("Unbound value: " ^ s)
          | ArgumentsMismatch(expected, got) -> error ("Wrong number of
          arguments provided! Expected " ^ string_of_int expected ^ " but got "
          ^ string_of_int got ^ "!")
          | TypeError -> error ("The object is not of the correct type")
  end;
  print_endline "";
  repl ()
end

let test_ltype = string_of_ltype (LCons(LInt(1), LCons(LInt(2), LCons(LInt(3), LUnit))))
let test_sexp = eval (Sexp([Value("cons"); Value("1"); Value("2")])) global_context
let test_cons = string_of_ltype test_sexp

let _ =
  repl ()
