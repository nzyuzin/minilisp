open Ltype

exception IllFormedSpecialForm of string
exception CannotEvaluate
exception NotApplicable of string
exception UnboundValue of string
exception ArgumentsMismatch of string * int

let rec eval (expression: ltype) (ctxt: context): ltype =
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
    eval_if if_expression ctxt
  | LCons(LIdentifier("cond"), _) as cond_expression -> eval_cond cond_expression ctxt
  | LCons(LIdentifier("begin"), _) as bgn ->
      validate_begin bgn;
      eval_sequence (ltype_cdr bgn) ctxt
  | LCons(LIdentifier("lambda"), _) as lambda ->
    validate_lambda lambda;
    eval_lambda lambda ctxt
  | LCons(LIdentifier("define"), _) as define ->
    validate_define define;
    eval_define define ctxt
  | LCons(LIdentifier("set!"), _) as set_expression ->
      validate_set set_expression;
      eval_set set_expression ctxt
  | LCons(f, arguments) ->
      let evaled_func = eval f ctxt in
      let evaled_arguments = ltype_map arguments (fun x -> eval x ctxt) in
      apply evaled_func evaled_arguments ctxt

and apply (func: ltype) (arguments: ltype) ctxt: ltype =
  match func with
  | LFunction(func) -> func arguments ctxt
  | _ -> raise (NotApplicable (string_of_ltype func))

and  validate_begin = function
  | LCons(LIdentifier("begin"), _) -> ()
  | x -> raise (IllFormedSpecialForm (string_of_ltype x))

and eval_sequence exprsns some_ctxt =
    match exprsns with
    | LCons(expr, LUnit) -> eval expr some_ctxt
    | LCons(expr, rest) ->
        let _ = eval expr some_ctxt in
        eval_sequence rest some_ctxt
    | x -> eval x some_ctxt

and validate_if = function
  | LCons(LIdentifier("if"),
      LCons(predicate, LCons(then_branch, LCons(else_branch, LUnit)))) -> ()
  | LCons(LIdentifier("if"), LCons(predicate, LCons(then_branch, LUnit))) -> ()
  | x -> raise (IllFormedSpecialForm (string_of_ltype x))

and eval_if if_expression ctxt =
  let predicate = ltype_car (ltype_cdr if_expression) in
  let then_branch = ltype_car (ltype_cdr (ltype_cdr if_expression)) in
  let else_branch = ltype_car (ltype_cdr (ltype_cdr (ltype_cdr if_expression))) in
  match eval predicate ctxt with
    | LBool(true) -> eval then_branch ctxt
    | LBool(false) -> eval else_branch ctxt
    | x -> raise (TypeError (x, "bool"))

and eval_cond cond_expression ctxt =
  let is_regular_clause = function
    | LCons(LIdentifier("else"), body) -> false
    | LCons(predicate, LCons(LIdentifier("=>"), body)) -> true
    | LCons(predicate, body) -> true
    | _ -> false in
  let is_else_clause = function
    | LCons(LIdentifier("else"), body) when body != LUnit -> true
    | _ -> false in
  let rec is_clauses = function
    | LUnit -> false
    | LCons(clause, LUnit)
      when is_regular_clause clause || (is_else_clause clause) -> true
    | LCons(clause, rest)
      when is_regular_clause clause && (not (is_else_clause clause)) && rest != LUnit ->
        is_clauses rest
    | x -> false in
  let validate_cond = function
    | LCons(LIdentifier("cond"), clauses) when is_clauses clauses -> ()
    | x -> raise (IllFormedSpecialForm (string_of_ltype x)) in
  let clause_predicate clause = ltype_car clause in
  let clause_body clause =
    let after_pred = ltype_cdr clause in
    if after_pred != LUnit && (ltype_car after_pred) = LIdentifier("=>") then
      ltype_cdr after_pred
    else after_pred in
  let rec eval_clauses clauses =
    let clause = ltype_car clauses in
    let rest_clauses = ltype_cdr clauses in
    if is_else_clause clause then eval_sequence (clause_body clause) ctxt
    else match eval (clause_predicate clause) ctxt with
      | LBool(true) as tru -> eval_sequence (LCons(tru, (clause_body clause))) ctxt
      | LBool(false) -> if rest_clauses = LUnit then LUnit else eval_clauses rest_clauses
      | x -> raise (TypeError (x, "bool")) in
  validate_cond cond_expression;
  eval_clauses (ltype_cdr cond_expression)

and validate_define = function
  | LCons(LIdentifier("define"), LCons(LCons(func_name, func_args), body))
    when is_identifier (unwrap_identifier func_name) -> ()
  | LCons(LIdentifier("define"), LCons(identifier, LCons(body, LUnit)))
    when is_identifier (unwrap_identifier identifier) -> ()
  | x -> raise (IllFormedSpecialForm (string_of_ltype x))

and eval_define dfn ctxt =
  let is_define_function = function
    | LCons(_, LCons(LCons(LIdentifier(identifier), _), _))
      when is_identifier identifier -> true
    | _ -> false in
  let eval_define_variable define =
    let identifier = ltype_car (ltype_cdr define) in
    let body = ltype_car (ltype_cdr (ltype_cdr define)) in
    let unwrapped_identifier = unwrap_identifier identifier in
    let ctxt_with_func_var = extend_context ctxt unwrapped_identifier identifier in
    let evaled_body = eval body ctxt_with_func_var in
    let new_ctxt = extend_context ctxt unwrapped_identifier evaled_body in
    ctxt_with_func_var := !new_ctxt;
    ctxt := !new_ctxt;
    identifier in
  let eval_define_function define =
    let func_name = ltype_car (ltype_car (ltype_cdr define)) in
    let fun_args = ltype_cdr (ltype_car (ltype_cdr define)) in
    let fun_body = ltype_cdr (ltype_cdr define) in
    (* transforms (define (f args) body) to (define f (lambda (args) body)) *)
    let lambda_transformation =
      LCons(LIdentifier("define"), LCons(func_name, LCons(
        LCons(LIdentifier("lambda"), LCons(fun_args, fun_body)), LUnit))) in
    eval_define_variable lambda_transformation in
  if is_define_function dfn then eval_define_function dfn
  else eval_define_variable dfn

and validate_set = function
  | LCons(LIdentifier("set!"), LCons(LIdentifier(identifier), LCons(value, LUnit)))
    when is_identifier identifier -> ()
  | x -> raise (IllFormedSpecialForm (string_of_ltype x))

and eval_set set_expression ctxt =
  let identifier = ltype_car (ltype_cdr set_expression) in
  let prev_value = eval identifier ctxt in
  let new_value = ltype_car (ltype_cdr (ltype_cdr set_expression)) in
  let define_transformation =
    LCons(LIdentifier("define"), LCons(identifier, LCons(new_value, LUnit))) in
  let _ = eval define_transformation ctxt in
  prev_value

and validate_lambda = function
  | LCons(LIdentifier("lambda"), LCons(lambda_args, lambda_body))
    when ltype_is_list lambda_args -> ()
  | x -> raise (IllFormedSpecialForm (string_of_ltype x))

and eval_lambda lambda ctxt =
  let lambda_args = ltype_car (ltype_cdr lambda) in
  let lambda_body = ltype_cdr (ltype_cdr lambda) in
  LFunction(fun args fctxt ->
    let provided_args_length = ltype_length args in
    let expected_args_length = ltype_length lambda_args in
    if provided_args_length != expected_args_length then
      raise (ArgumentsMismatch(string_of_int expected_args_length, provided_args_length))
    else
      eval_sequence lambda_body (add_variables_to_context ctxt lambda_args args))
