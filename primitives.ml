open Ltype

let bad_arguments expected actual =
  let expected_str = if expected < 0 then ("at least " ^ (string_of_int (-expected)))
    else string_of_int expected in
  raise (Evaluator.ArgumentsMismatch (expected_str, actual))

let check_length_equality arguments expected =
    let length = ltype_length arguments in
    if length = expected then
      ()
    else bad_arguments expected length

let any_arguments_function combinator initial_value: ltype =
  LFunction(fun arguments ctxt -> ltype_foldr combinator arguments initial_value)

let zero_or_one_argument_function f: ltype =
  LFunction(fun arguments ctxt ->
    let length = ltype_length arguments in
    if length > 0 then
      if length = 1 then
        f (Some (ltype_car arguments))
      else
        raise (Evaluator.ArgumentsMismatch ("between 0 and 1", length))
    else
      f None)

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
    let _ = check_length_equality arguments 1 in
    f (ltype_car arguments))

let two_arguments_function (func : 'a -> 'b -> ltype): ltype =
  LFunction(fun arguments ctxt ->
    let _ = check_length_equality arguments 2 in
    let f = ltype_car arguments in
    let s = ltype_car (ltype_cdr arguments) in
    func f s)

let to_ltype_ints_combinator (operator: int -> int -> int): ltype -> ltype -> ltype =
  fun f s -> LInt(operator (int_of_ltype f) (int_of_ltype s))

let global_context: context = ref
  [
    ("+", any_arguments_function (to_ltype_ints_combinator ( + )) (LInt 0));
    ("-", at_least_one_argument_function (to_ltype_ints_combinator ( - )) (LInt 0));
    ("*", any_arguments_function (to_ltype_ints_combinator ( * )) (LInt 1));
    ("/", at_least_one_argument_function (to_ltype_ints_combinator ( / )) (LInt 1));
    ("=", two_arguments_function (fun f s -> LBool((int_of_ltype f) = (int_of_ltype s))));
    ("<", two_arguments_function (fun f s -> LBool((int_of_ltype f) < (int_of_ltype s))));
    ("<=", two_arguments_function (fun f s -> LBool((int_of_ltype f) <= (int_of_ltype s))));
    (">", two_arguments_function (fun f s -> LBool((int_of_ltype f) > (int_of_ltype s))));
    (">=", two_arguments_function (fun f s -> LBool((int_of_ltype f) >= (int_of_ltype s))));
    ("eq?", two_arguments_function (fun f s -> LBool(f = s)));
    ("and", two_arguments_function (fun f s -> LBool((bool_of_ltype f) && (bool_of_ltype s))));
    ("or", two_arguments_function (fun f s -> LBool((bool_of_ltype f) || (bool_of_ltype s))));
    ("cons", two_arguments_function (fun f s -> LCons(f, s)));
    ("car", one_argument_function ltype_car);
    ("cdr", one_argument_function ltype_cdr);
    ("null?", one_argument_function (fun x -> LBool(x = LUnit)));
    ("list", LFunction(fun arguments ctxt -> arguments));
    ("display", one_argument_function (fun lstr -> print_endline (string_of_ltype lstr); LUnit));
    ("newline", zero_or_one_argument_function (fun x -> print_endline ""; LUnit));
    ("exit", zero_or_one_argument_function (function
                                            | None -> exit 0
                                            | Some num -> exit (int_of_ltype num)));
  ]
