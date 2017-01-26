open Ltype

let bad_arguments expected actual =
  raise (Evaluator.ArgumentsMismatch (expected, actual))

let any_arguments_function combinator initial_value: ltype =
  LFunction(fun arguments ctxt -> ltype_foldr combinator arguments initial_value)

let at_least_one_argument_function combinator initial_value: ltype =
  LFunction(fun arguments ctxt ->
    let length = ltype_length arguments in
    if length >= 1 then
      if length = 1 then
        ltype_foldl combinator arguments initial_value
      else
        ltype_foldl combinator (ltype_cdr arguments) (ltype_car arguments)
    else bad_arguments (-1) length)

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
  | v -> raise (TypeError (v, "int"))

let to_ltype_ints_combinator (operator: int -> int -> int): ltype -> ltype -> ltype =
  fun f s -> LInt(operator (int_of_ltype f) (int_of_ltype s))

let global_context: context = ref
  [("+", any_arguments_function (to_ltype_ints_combinator ( + )) (LInt 0));
   ("-", at_least_one_argument_function (to_ltype_ints_combinator ( - )) (LInt 0));
   ("*", any_arguments_function (to_ltype_ints_combinator ( * )) (LInt 1));
   ("/", at_least_one_argument_function (to_ltype_ints_combinator ( / )) (LInt 1));
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
