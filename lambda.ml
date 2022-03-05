open Lib

let rec read_channel c ~verbose ~exiting=
  let lexbuf = Lexing.from_channel c in
  let eval = if verbose then Lambda_util.evaluate_exp else Lambda_util.evaluate_exp_silent in
  while true do
    try
      eval (Parser.main Lexer.token lexbuf);
    with 
    | Lexer.EOF -> if exiting then exit 0 else read_channel stdin ~verbose: true ~exiting: true;
    | Parsing.Parse_error -> print_endline "--> Syntax Error";
  done;
;;

let read_file path =
  let f = open_in path in
  read_channel f ~verbose: false ~exiting: false;
  close_in f;
;;
    
let run () =
  (*TODO: use array directly ?*)
  Sys.argv |> Array.to_list |> List.tl |> List.iter read_file;
  read_channel stdin ~verbose: true ~exiting: true;
;;

let () = run ()