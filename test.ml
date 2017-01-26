exception TestFailed of string

let verbose = ref false
let anon_arguments: string list ref = ref []

let command_line_arguments = [
  ("-v", Arg.Set(verbose), "Be verbose about tests")
]

let eval expression =
  let parsed_input = Parser.parse_input expression in
  Evaluator.eval parsed_input Primitives.global_context

(* TODO: write a function that will read a test_file from stdin
 * and apply test on it *)
let test (expression, expected_result): unit =
  let test_success_message () =
    "Successful test: eval(" ^ expression ^ ") = " ^ (Ltype.string_of_ltype expected_result) in
  let test_failure_message actual_result =
      "Test failed on expression " ^ expression ^ "! expected "
        ^ (Ltype.string_of_ltype expected_result) ^ " but got "
        ^ (Ltype.string_of_ltype actual_result) ^ "." in
  let evaled_input = eval expression in
  if evaled_input = expected_result then
    if !verbose then print_endline (test_success_message ())
    else ()
  else
    print_endline (test_failure_message evaled_input)

let test_file filename expected_result =
  let evaluation_result = Repl.load_file filename (not !verbose) in
  match evaluation_result with
  | Some res ->
      if !verbose then
        print_endline ("Result of evaluating " ^ filename ^ ": " ^ (Ltype.string_of_ltype res));
      res = expected_result
  | None -> false

let check_arguments (arguments: string list): bool =
  let f = List.hd arguments in
  if List.length arguments > 1 then
    let s = List.hd (List.tl arguments) in
    let result = test_file f (eval s) in
    print_endline (string_of_bool (result));
    result
  else
    test_file f Ltype.LUnit

let usage = "Usage: " ^ Sys.argv.(0) ^ " [-v] test_file [expected_result]"

let _ = begin
  Arg.parse command_line_arguments
    (fun x -> anon_arguments := (List.append !anon_arguments [x]))
    usage;
  let _ = Repl.load_stdlib () in
  if List.length !anon_arguments >= 1 then
    let _ = check_arguments !anon_arguments in
    ()
  else
    Arg.usage command_line_arguments usage
end
