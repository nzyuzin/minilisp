open Ltype;;

exception TestFailed of string

type test_expression = string * ltype

let verbose = ref false

let command_line_arguments = [
  ("-v", Arg.Bool(fun b -> verbose := b), "Print more information about tests")
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

let test (expression, expected_result): unit =
  let parsed_input = Parser.parse_input expression in
  let evaled_input = Evaluator.eval parsed_input Primitives.global_context in
  if evaled_input = expected_result then ()
  else begin
      print_endline ("Test failed on expression " ^ expression
        ^ "! expected " ^ (string_of_ltype expected_result) ^ " but got "
        ^ (string_of_ltype evaled_input) ^ ".");
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

let _ = begin
  Arg.parse command_line_arguments (fun x -> ()) ("Usage: " ^ Sys.argv.(0) ^ " [-v]");
  Repl.load_stdlib ();
  test_all ()
end
