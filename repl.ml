exception NotSexp of string
exception NotMatchingBraces
exception NotApplicable of string
exception CannotEvaluate
exception UnboundValue of string
exception CannotCast of string * string
exception ArgumentsMismatch of int * int
exception TypeError
exception IllFormedSpecialForm of string

type 'b sexp = Value of 'b | Sexp of 'b sexp list

type ltype =
  | LUnit
  | LInt of int
  | LBool of bool
  | LString of string
  | LCons of ltype * ltype
  | LIdentifier of string
  | LFunction of (ltype -> context -> ltype)
and context = (string * ltype) list ref

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

let after_first_char str =
  String.sub str 1 (String.length str - 1)

let inside_char_bounds chr f s =
  let code = Char.code chr in
  code >= (Char.code f) && code <= (Char.code s)

let is_digit chr =
  inside_char_bounds chr '0' '9'

let is_alpha chr =
  (inside_char_bounds chr 'a' 'z') || (inside_char_bounds chr 'A' 'Z')

let is_alphanum chr =
  is_digit chr || is_alpha chr

let is_identifier str: bool =
  let allowed_symbols = "_-?!*/+=<>@$%^:~" in
  let is_allowed_symbol chr =
    is_alphanum chr || String.contains allowed_symbols chr in
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

let rec ltype_of_sexp (expression: string sexp): ltype =
  let rec is_int (str: string) =
    let rec is_digits s =
      let length = String.length s in
      if length = 0 then false
      else if length = 1 then is_digit s.[0]
      else is_digit s.[0] && is_digits (after_first_char s) in
    if str.[0] = '-' && String.length str > 1 then
      is_digits (after_first_char str)
    else
      is_digits str in
  let is_bool (x: string) =
    x = "#t" || x = "#f" in
  let is_string (x: string) =
    let length = String.length x in
    (x.[0] = '\'' && x.[length - 1] = '\'')
    || (x.[0] = '"' && x.[length - 1] = '"') in
  let bool_of_ltype_string b =
    if b = "#t" then true
    else if b = "#f" then false
    else raise (CannotCast (b, "bool")) in
  let rec ltype_of_sexp_list = function
    | [] -> LUnit
    | x :: xs -> LCons(ltype_of_sexp x, ltype_of_sexp_list xs) in
  match expression with
  | Value(v) ->
      if is_int v then LInt(int_of_string v)
      else if is_bool v then LBool(bool_of_ltype_string v)
      else if is_string v then LString(v)
      else if is_identifier v then LIdentifier(v)
      else raise (Failure ("Cannot convert sexp to ltype: " ^ v))
  | Sexp(c) -> begin match c with
    | [] -> LUnit
    | x :: xs -> LCons(ltype_of_sexp x, ltype_of_sexp_list xs)
  end

let rec ltype_is_list = function
  | LUnit -> true
  | LCons(_, xs) -> ltype_is_list xs
  | _ -> false

let rec ltype_map l f =
  match l with
  | LUnit -> LUnit
  | LCons(x, xs) -> LCons(f(x), ltype_map xs f)
  | _ -> raise TypeError

let ltype_car = function
  | LCons(x, _) -> x
  | _ -> raise TypeError

let ltype_cdr = function
  | LCons(_, xs) -> xs
  | _ -> raise TypeError

let rec ltype_length = function
  | LUnit -> 0
  | LCons(_, xs) -> 1 + ltype_length xs
  | _ -> raise TypeError

let rec lookup (ctxt: context) (name: string) : ltype option =
  match !ctxt with
  | [] -> None
  | (name', value) :: xs -> if name = name' then (Some value) else lookup (ref xs) name

let extend_context (ctxt: context) (name: string) (value: ltype): context =
  ref ((name, value) :: !ctxt)

let unwrap_identifier = function
  | LIdentifier(name) -> name
  | _ -> raise TypeError

let rec add_variables_to_context ctxt vars vals: context =
  match vars with
  | LUnit -> ctxt
  | LCons(var, rest) ->
      let new_ctxt = extend_context ctxt (unwrap_identifier var) (ltype_car vals) in
      add_variables_to_context new_ctxt rest (ltype_cdr vals)
  | _ -> raise TypeError

let rec eval (expression: ltype) (ctxt: context): ltype =
  let validate_if = function
    | LCons(LIdentifier("if"),
        LCons(predicate, LCons(then_branch, LCons(else_branch, LUnit)))) -> ()
    | x -> raise (IllFormedSpecialForm (string_of_ltype x)) in
  let validate_lambda = function
    | LCons(LIdentifier("lambda"), LCons(lambda_args, LCons(lambda_body, LUnit)))
      when ltype_is_list lambda_args -> ()
    | x -> raise (IllFormedSpecialForm (string_of_ltype x)) in
  let validate_define = function
    | LCons(LIdentifier("define"), LCons(identifier, body))
      when is_identifier (unwrap_identifier identifier) -> ()
    | x -> raise (IllFormedSpecialForm (string_of_ltype x)) in
  match expression with
  | LUnit -> LUnit
  | LInt(_) as x -> x
  | LBool(_) as x -> x
  | LString(_) as x -> x
  | LIdentifier(v) -> begin match lookup ctxt v with
    | Some value  -> value
    | None -> raise (UnboundValue v)
  end
  | LFunction(func) -> LFunction(func)
  | LCons(LIdentifier("if"), _) as if_expression ->
    validate_if if_expression;
    let predicate = ltype_car (ltype_cdr if_expression) in
    let then_branch = ltype_car (ltype_cdr (ltype_cdr if_expression)) in
    let else_branch = ltype_car (ltype_cdr (ltype_cdr (ltype_cdr if_expression))) in
    begin match (eval predicate ctxt) with
      | LBool(true) -> eval then_branch ctxt
      | LBool(false) -> eval else_branch ctxt
      | _ -> raise TypeError
    end
  | LCons(LIdentifier("lambda"), _) as lambda ->
    validate_lambda lambda;
    let lambda_args = ltype_car (ltype_cdr lambda) in
    let lambda_body = ltype_car (ltype_cdr (ltype_cdr lambda)) in
    LFunction(fun args fctxt ->
      let provided_args_length = ltype_length args in
      let expected_args_length = ltype_length lambda_args in
      if provided_args_length != expected_args_length then
        raise (ArgumentsMismatch(expected_args_length, provided_args_length))
      else
        eval lambda_body (add_variables_to_context ctxt lambda_args args))
  | LCons(LIdentifier("define"), _) as define ->
    validate_define define;
    let identifier = ltype_car (ltype_cdr define) in
    let body = ltype_car (ltype_cdr (ltype_cdr define)) in
    let unwrapped_identifier = unwrap_identifier identifier in
    let ctxt_with_func_var = extend_context ctxt unwrapped_identifier identifier in
    let evaled_body = eval body ctxt_with_func_var in
    let new_ctxt = extend_context ctxt unwrapped_identifier evaled_body in
    ctxt_with_func_var := !new_ctxt;
    ctxt := !new_ctxt;
    identifier
  | LCons(f, rest) -> apply f rest ctxt

and apply (f: ltype) (arguments: ltype) ctxt: ltype =
  let evaled_func = eval f ctxt in
  let evaled_arguments: ltype = ltype_map arguments (fun x -> eval x ctxt) in
  match evaled_func with
  | LFunction(func) -> func evaled_arguments ctxt
  | _ -> raise (NotApplicable (string_of_ltype f))

let rec parse_input (source: unit -> string): string sexp =
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
    let rec inner str braces =
      let length = String.length str in
      if length = 0 then
        if braces = 0 then ""
        else if braces > 0 then inner (" " ^ source ()) braces
        else raise NotMatchingBraces
      else
        let first_symbol = String.sub str 0 1 in
        let rest = after_first_char str in
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
        if str.[0] = ' ' then trim_head (after_first_char str)
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
  and parse_sexp (str: string): string sexp =
    let length = String.length str in
    let rec no_spaces s =
      let s_length = String.length s in
      if s_length = 0 then
        true
      else if s.[0] = ' ' then false
      else no_spaces (after_first_char s) in
    let balanced_braces s =
      let rec inner pos balance =
        if pos = String.length s then balance
        else if s.[pos] = '(' then inner (pos + 1) (balance + 1)
        else if s.[pos] = ')' then inner (pos + 1) (balance - 1)
        else inner (pos + 1) balance in
      inner 0 0 = 0 in
    if length = 0 then Sexp([])
    else if str.[0] = '(' && str.[length - 1] = ')' && balanced_braces str then
      Sexp(parse_cons (trim (String.sub str 1 (length - 2))))
    else if no_spaces str then
      Value(read_word str)
    else
      raise (NotSexp str) in
  parse_sexp (read_word (trim (surround_braces (source ()))))

let bad_arguments expected actual =
  raise (ArgumentsMismatch(expected, actual))

let one_argument_function f: ltype =
  LFunction(fun arguments ctxt ->
    let length = ltype_length arguments in
    if length = 1 then
      f (ltype_car arguments)
    else bad_arguments 1 length)

let two_arguments_function (func : 'a -> 'b -> ltype): ltype =
  LFunction(fun arguments ctxt ->
    let length = ltype_length arguments in
    if length = 2 then
      let f = ltype_car arguments in
      let s = ltype_car (ltype_cdr arguments) in
      func f s
    else bad_arguments 2 length)

let int_of_ltype = function
  | LInt(i) -> i
  | v -> raise (CannotCast (string_of_ltype v, "int"))

let two_ints_function (operator: int -> int -> int): ltype =
  two_arguments_function (fun f s -> LInt(operator (int_of_ltype f) (int_of_ltype s)))

let global_context: context = ref
  [("+", two_ints_function ( + ));
   ("-", two_ints_function ( - ));
   ("*", two_ints_function ( * ));
   ("/", two_ints_function ( / ));
   ("=", two_arguments_function (fun f s -> LBool((int_of_ltype f) = (int_of_ltype s))));
   ("<", two_arguments_function (fun f s -> LBool((int_of_ltype f) < (int_of_ltype s))));
   ("<=", two_arguments_function (fun f s -> LBool((int_of_ltype f) <= (int_of_ltype s))));
   (">", two_arguments_function (fun f s -> LBool((int_of_ltype f) > (int_of_ltype s))));
   (">=", two_arguments_function (fun f s -> LBool((int_of_ltype f) >= (int_of_ltype s))));
   ("cons", two_arguments_function (fun f s -> LCons(f, s)));
   ("car", one_argument_function ltype_car);
   ("cdr", one_argument_function ltype_cdr);
   ("nil?", one_argument_function (fun x -> LBool(x = LUnit)));
   ("list", LFunction(fun arguments ctxt -> arguments))]


let dummy_sexp = (Sexp([Value("cons"); Value("1"); Value("2")]))
let test_ltype = string_of_ltype (LCons(LInt(1), LCons(LInt(2), LCons(LInt(3), LUnit))))
let test_sexp = eval (ltype_of_sexp dummy_sexp) global_context
let test_cons = string_of_ltype test_sexp
let dummy_define_lambda = "(define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))"
let dummy_define_lambda_sexp = parse_input (fun () -> dummy_define_lambda)
let dummy_define_lambda_tokens = ltype_of_sexp dummy_define_lambda_sexp
let test_define_lambda = eval dummy_define_lambda_tokens global_context

let _ =
  let error str = prerr_endline ("Error: " ^ str) in
  let print_caret () = print_string "/> " in
  let read_line () = input_line stdin in
  let read source exit_on_eof: string sexp option =
    let parsed_input = ref None in
    let try_read =
      try
        parsed_input := Some (parse_input source)
      with
        | NotSexp(str) -> error ("Not an S-expression: " ^ str ^ "!")
        | NotMatchingBraces -> error "Not matching amount of braces!"
        | End_of_file ->
            if exit_on_eof then begin
              print_endline "";
              print_endline "End of input stream reached.";
              exit 0;
            end
            else raise End_of_file in
    try_read;
    !parsed_input in
  let eval_print parsing_result be_quiet =
    match parsing_result with
    | None -> ();
    | Some parsed_sexp ->
      try
        let tokenized_input = ltype_of_sexp parsed_sexp in
        let evaled_input = eval tokenized_input global_context in
        if be_quiet then ()
        else print_endline (";Value: " ^ (string_of_ltype evaled_input))
      with
        | NotApplicable(str) -> error ("The object " ^ str ^ " is not applicable!")
        | CannotEvaluate -> error "Expression cannot be evaluated!"
        | CannotCast(value, to_type) ->
            error ("Cannot cast " ^ value ^ " to type " ^ to_type ^ "!")
        | UnboundValue(s) -> error ("Unbound value: " ^ s)
        | ArgumentsMismatch(expected, got) -> error ("Wrong number of "
            ^ "arguments provided: Expected " ^ string_of_int expected
            ^ " but got " ^ string_of_int got ^ "!")
        | TypeError -> error ("The object is not of the correct type")
        | IllFormedSpecialForm(text) -> error ("Ill-formed special form: " ^ text) in
  let load_stdlib () =
    let stdlib_filename = "stdlib.minilisp" in
    let rec eval_whole_file source =
      try
        eval_print (read source false) true;
        eval_whole_file source
      with
        | End_of_file -> () in
    if Sys.file_exists stdlib_filename then
      let input = open_in stdlib_filename in
      eval_whole_file (fun () -> input_line input);
      close_in input
    else
      () in
  let rec repl () = begin
    print_caret ();
    flush stdout;
    eval_print (read read_line true) false;
    print_endline "";
    repl ()
  end in
  load_stdlib ();
  repl ()
