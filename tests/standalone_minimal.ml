(*== Build the example with:

"ocamlopt -o standalone_minimal -I `ocamlfind query psmt2-frontend`
 psmt2Frontend.cmxa standalone_minimal.ml"

if the lib is installed, or with:

"ocamlopt -o standalone_minimal -I ../src psmt2Frontend.cmxa
 standalone_minimal.ml"

if the lib is built but not installed

==*)

open Format

module Smtlib_error = Psmt2Frontend.Smtlib_error
module Options = Psmt2Frontend.Options
module Smtlib_parser = Psmt2Frontend.Smtlib_parser
module Smtlib_lexer = Psmt2Frontend.Smtlib_lexer
module Smtlib_typing = Psmt2Frontend.Smtlib_typing

let fmt = err_formatter

let verbose = ref 0
let parse_only = ref false
let quiet = ref false

let smt2 = ".smt2"
let psmt2 = ".psmt2"

let usage = sprintf "usage: %s [options] file%s" Sys.argv.(1) smt2

let spec =
  [
   "-parse-only", Arg.Set parse_only, "  stops after parsing";
   "-quiet", Arg.Set quiet, "  don't print warning";
   "-verbose", Arg.Set_int verbose, "  1 : print typed ast, 2 : print typing env"
  ]

let file =
  let file = ref None in
  let set_file s =
    if (not (Filename.check_suffix s psmt2)) &&
      (not (Filename.check_suffix s smt2)) then
      raise (Arg.Bad "invalid extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  Options.set_quiet !quiet;
  Options.set_verbose !verbose;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
  try
    let in_chan = open_in file in
    let lexbuf = Lexing.from_channel in_chan in
    try
      let parsed = ref (Smtlib_parser.commands Smtlib_lexer.token lexbuf) in
      if not !parse_only then
        Smtlib_typing.typing !parsed;
      if not (Options.quiet ()) then
        printf "%s@." (Options.status ());
      exit 0
    with
    | Smtlib_parser.Error ->
      let loc = Smtlib_lexer.current_pos lexbuf in
      Smtlib_error.print fmt file (Syntax_error (Lexing.lexeme lexbuf)) loc;
      exit 1
    |Smtlib_error.Error (e,p) ->
      Smtlib_error.print fmt file e p;
      exit 1
  with
  |Invalid_argument _ ->
    fprintf fmt "No input file given@.";
    exit 1
