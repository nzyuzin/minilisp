let error str = begin
  prerr_endline (";Error: " ^ str);
  None
end
let print_caret () = print_string "/> "
let read_line () = input_line stdin

let read source exit_on_eof: Ltype.ltype option =
  let rec read_until_balance src already_read =
    let str = String.trim (Parser.remove_comments (src ())) in
    if String.length str = 0 then
      read_until_balance src already_read
    else
      let new_read = already_read ^ str in
      if Parser.balanced_braces new_read then
        new_read
      else
        read_until_balance src new_read in
  try
    let input_str = read_until_balance source "" in
    Some (Parser.parse_input input_str)
  with
    | Parser.NotSexp str -> error ("Not an S-expression: " ^ str)
    | Parser.NotMatchingBraces -> error "Not matching amount of braces."
    | Parser.InvalidIdentifier str -> error ("Invalid identifier: " ^ str)
    | End_of_file ->
        if exit_on_eof then begin
          print_endline "";
          print_endline "End of input stream reached.";
          exit 0;
        end
        else raise End_of_file

let eval_print parsing_result be_quiet =
  match parsing_result with
  | None -> None;
  | Some tokenized_input ->
    try
      let evaled_input = Evaluator.eval tokenized_input Primitives.global_context in
      if be_quiet then ()
      else print_endline (";Value: " ^ (Ltype.string_of_ltype evaled_input));
      Some evaled_input
    with
      | Evaluator.NotApplicable(str) -> error ("The object " ^ str ^ " is not applicable.")
      | Evaluator.CannotEvaluate -> error "Expression cannot be evaluated."
      | Evaluator.UnboundValue(s) -> error ("Unbound value: " ^ s)
      | Evaluator.ArgumentsMismatch(expected, got) -> error ("Wrong number of "
          ^ "arguments provided: Expected " ^
          (if expected < 0 then "at least " ^ (string_of_int (-expected))
          else string_of_int expected)
          ^ " but got " ^ string_of_int got ^ "!")
      | Ltype.TypeError(actual, expected_type) ->
          error ("The object " ^ (Ltype.string_of_ltype actual)
            ^ " is not of expected type " ^ expected_type)
      | Evaluator.IllFormedSpecialForm(text) -> error ("Ill-formed special form: " ^ text)

let load_file filename quiet =
  let rec eval_whole_file source prev_result =
    try
      let evaluation_result = eval_print (read source false) quiet in
      eval_whole_file source evaluation_result
    with
      | End_of_file -> prev_result in
  if Sys.file_exists filename then
    let input = open_in filename in
    let result = eval_whole_file (fun () -> input_line input) None in
    close_in input;
    result
  else
    None

let load_stdlib () =
  let stdlib_filename = "stdlib.minilisp" in
  load_file stdlib_filename true

let rec repl () = begin
  print_caret ();
  flush stdout;
  let _ = eval_print (read read_line true) false in
  print_endline "";
  repl ()
end
