open Ltype

exception IllFormedSpecialForm of string
exception CannotEvaluate
exception NotApplicable of string
exception UnboundValue of string
exception ArgumentsMismatch of int * int

let rec lookup (ctxt: context) (name: string) : ltype option =
  match !ctxt with
  | [] -> None
  | (name', value) :: xs -> if name = name' then (Some value) else lookup (ref xs) name

let extend_context (ctxt: context) (name: string) (value: ltype): context =
  ref ((name, value) :: !ctxt)

let unwrap_identifier = function
  | LIdentifier(name) -> name
  | x -> raise (TypeError (x, "list"))

let rec add_variables_to_context ctxt vars vals: context =
  match vars with
  | LUnit -> ctxt
  | LCons(var, rest) ->
      let new_ctxt = extend_context ctxt (unwrap_identifier var) (ltype_car vals) in
      add_variables_to_context new_ctxt rest (ltype_cdr vals)
  | x -> raise (TypeError (x, "list"))

let rec eval (expression: ltype) (ctxt: context): ltype =
  let validate_if = function
    | LCons(LIdentifier("if"),
        LCons(predicate, LCons(then_branch, LCons(else_branch, LUnit)))) -> ()
    | x -> raise (IllFormedSpecialForm (string_of_ltype x)) in
  let eval_if if_expression =
    let predicate = ltype_car (ltype_cdr if_expression) in
    let then_branch = ltype_car (ltype_cdr (ltype_cdr if_expression)) in
    let else_branch = ltype_car (ltype_cdr (ltype_cdr (ltype_cdr if_expression))) in
    match eval predicate ctxt with
      | LBool(true) -> eval then_branch ctxt
      | LBool(false) -> eval else_branch ctxt
      | x -> raise (TypeError (x, "bool")) in
  let validate_define = function
    | LCons(LIdentifier("define"), LCons(identifier, body))
      when is_identifier (unwrap_identifier identifier) -> ()
    | x -> raise (IllFormedSpecialForm (string_of_ltype x)) in
  let eval_define define =
    let identifier = ltype_car (ltype_cdr define) in
    let body = ltype_car (ltype_cdr (ltype_cdr define)) in
    let unwrapped_identifier = unwrap_identifier identifier in
    let ctxt_with_func_var = extend_context ctxt unwrapped_identifier identifier in
    let evaled_body = eval body ctxt_with_func_var in
    let new_ctxt = extend_context ctxt unwrapped_identifier evaled_body in
    ctxt_with_func_var := !new_ctxt;
    ctxt := !new_ctxt;
    identifier in
  let validate_lambda = function
    | LCons(LIdentifier("lambda"), LCons(lambda_args, LCons(lambda_body, LUnit)))
      when ltype_is_list lambda_args -> ()
    | x -> raise (IllFormedSpecialForm (string_of_ltype x)) in
  let eval_lambda lambda =
    let lambda_args = ltype_car (ltype_cdr lambda) in
    let lambda_body = ltype_car (ltype_cdr (ltype_cdr lambda)) in
    LFunction(fun args fctxt ->
      let provided_args_length = ltype_length args in
      let expected_args_length = ltype_length lambda_args in
      if provided_args_length != expected_args_length then
        raise (ArgumentsMismatch(expected_args_length, provided_args_length))
      else
        eval lambda_body (add_variables_to_context ctxt lambda_args args)) in
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
    eval_if if_expression
  | LCons(LIdentifier("lambda"), _) as lambda ->
    validate_lambda lambda;
    eval_lambda lambda
  | LCons(LIdentifier("define"), _) as define ->
    validate_define define;
    eval_define define
  | LCons(f, rest) -> apply f rest ctxt

and apply (f: ltype) (arguments: ltype) ctxt: ltype =
  let evaled_func = eval f ctxt in
  let evaled_arguments: ltype = ltype_map arguments (fun x -> eval x ctxt) in
  match evaled_func with
  | LFunction(func) -> func evaled_arguments ctxt
  | _ -> raise (NotApplicable (string_of_ltype evaled_func))
