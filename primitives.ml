open Ltype;;
open Evaluator;;

let bad_arguments expected actual =
  raise (ArgumentsMismatch(expected, actual))

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

let two_ints_function (operator: int -> int -> int): ltype =
  two_arguments_function (fun f s -> LInt(operator (int_of_ltype f) (int_of_ltype s)))

let global_context: context = ref
  [("+", two_ints_function ( + ));
   ("-", two_ints_function ( - ));
   ("*", two_ints_function ( * ));
   ("/", two_ints_function ( / ));
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
