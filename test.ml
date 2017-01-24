open Ltype

exception TestFailed of string

type test_expression = string * ltype

let verbose = ref false
let anon_arguments: string list ref = ref []

let command_line_arguments = [
  ("-v", Arg.Set(verbose), "Be verbose about tests")
]

let dummy_define_lambda = "(define factorial (lambda (x) (if (= x 0) 1 (* x (factorial (- x 1))))))"
let dummy_define_lambda_tokens = Parser.parse_input dummy_define_lambda
let test_define_lambda = Evaluator.eval dummy_define_lambda_tokens Primitives.global_context

let test_expressions = [
  ("2", (LInt 2));
  ("(cons 1 2)", (LCons ((LInt 1), LInt(2))));
  ("(cons 1 2) ; with comments!", (LCons ((LInt 1), LInt(2))));
  ("(factorial 5)", (LInt 120));
]

let completed_tests = ref (List.length test_expressions)

let eval expression =
  let parsed_input = Parser.parse_input expression in
  Evaluator.eval parsed_input Primitives.global_context

let test (expression, expected_result): unit =
  let test_success_message () =
    "Successful test: eval(" ^ expression ^ ") = " ^ (string_of_ltype expected_result) in
  let test_failure_message actual_result =
      "Test failed on expression " ^ expression ^ "! expected "
        ^ (string_of_ltype expected_result) ^ " but got "
        ^ (string_of_ltype actual_result) ^ "." in
  let evaled_input = eval expression in
  if evaled_input = expected_result then
    if !verbose then print_endline (test_success_message ())
    else ()
  else begin
      print_endline (test_failure_message evaled_input);
      completed_tests := (!completed_tests - 1)
    end

let test_all () =
  let rec inner = function
    | [] ->
      let length = List.length test_expressions in
      if !completed_tests = length then
        print_endline "All tests completed!"
      else
        print_endline ((string_of_int !completed_tests) ^ " tests out of "
          ^ (string_of_int length) ^ " completed! "
          ^ (string_of_int (length - !completed_tests)) ^ " failed.")
    | x :: xs -> begin
        test x;
        inner xs
      end in
  inner test_expressions

let test_file filename expected_result =
  let evaluation_result = Repl.load_file filename (not !verbose) in
  match evaluation_result with
  | Some res ->
      if !verbose then
        print_endline ("Result of evaluating " ^ filename ^ ": " ^ (string_of_ltype res));
      res = expected_result
  | None -> false

let check_arguments (arguments: string list): bool =
  let f = List.hd arguments in
  let s = List.hd (List.tl arguments) in
  test_file f (eval s)

let _ = begin
  Arg.parse command_line_arguments
    (fun x -> anon_arguments := (List.append !anon_arguments [x]))
    ("Usage: " ^ Sys.argv.(0) ^ " [-v]");
  let _ = Repl.load_stdlib () in
  if List.length !anon_arguments >= 2 then
    print_endline (string_of_bool (check_arguments !anon_arguments))
  else
    test_all ();
end
