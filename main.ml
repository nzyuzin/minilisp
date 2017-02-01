let file_before_repl: string ref = ref ""
let verbose: bool ref = ref false

let command_line_arguments = [
  ("-f", Arg.Set_string(file_before_repl), "Load specified file before starting repl");
  ("-v", Arg.Set(verbose), "Execute specified files verbosely");
]

let usage = "Usage: " ^ Sys.argv.(0) ^ " [-f filename] [-v] [filename]"

let _ =
  let _ = Repl.load_stdlib () in
  let file_to_execute: string ref = ref "" in
  let _ = Arg.parse command_line_arguments
    (fun filename -> file_to_execute := filename)
    usage in
    if String.length !file_before_repl != 0 then
      let _ = Repl.load_file !file_before_repl (not !verbose) in
      ()
    else
      ();
    if String.length !file_to_execute != 0 then
      let _ = Repl.load_file !file_to_execute (not !verbose) in
      ()
    else
      Repl.repl ()
