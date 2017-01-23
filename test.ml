open Ltype;;
open Parser;;
open Evaluator;;
open Primitives;;

let dummy_sexp = (Sexp([Value("cons"); Value("1"); Value("2")]))
let test_ltype = string_of_ltype (LCons(LInt(1), LCons(LInt(2), LCons(LInt(3), LUnit))))
let test_sexp = eval (ltype_of_sexp dummy_sexp) global_context
let test_cons = string_of_ltype test_sexp
let dummy_define_lambda = "(define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))"
let dummy_define_lambda_sexp = parse_input (fun () -> dummy_define_lambda)
let dummy_define_lambda_tokens = ltype_of_sexp dummy_define_lambda_sexp
let test_define_lambda = eval dummy_define_lambda_tokens global_context
