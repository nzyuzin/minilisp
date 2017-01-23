open Parser;;
open Ltype;;
open Evaluator;;
open Primitives;;

let error str = prerr_endline (";Error: " ^ str)
let print_caret () = print_string "/> "
let read_line () = input_line stdin

let read source exit_on_eof: ltype option =
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
  begin
    try_read;
    !parsed_input
  end

let eval_print parsing_result be_quiet =
  match parsing_result with
  | None -> ();
  | Some tokenized_input ->
    try
      let evaled_input = eval tokenized_input global_context in
      if be_quiet then ()
      else print_endline (";Value: " ^ (string_of_ltype evaled_input))
    with
      | NotApplicable(str) -> error ("The object " ^ str ^ " is not applicable!")
      | CannotEvaluate -> error "Expression cannot be evaluated!"
      | UnboundValue(s) -> error ("Unbound value: " ^ s)
      | ArgumentsMismatch(expected, got) -> error ("Wrong number of "
          ^ "arguments provided: Expected " ^ string_of_int expected
          ^ " but got " ^ string_of_int got ^ "!")
      | TypeError(actual, expected_type) ->
          error ("The object " ^ (string_of_ltype actual)
            ^ " is not of expected type " ^ expected_type)
      | IllFormedSpecialForm(text) -> error ("Ill-formed special form: " ^ text)

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
    ()

let rec repl () = begin
  print_caret ();
  flush stdout;
  eval_print (read read_line true) false;
  print_endline "";
  repl ()
end
