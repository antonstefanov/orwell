module Ast = Ast

type position = { fname : string; ln : int; col : int }

type parse_error = SyntaxError of position * string | ParserError of position

let to_position (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  { fname = pos.pos_fname; ln = pos.pos_lnum; col = pos.pos_cnum - pos.pos_bol }

let position_to_string (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_line (lexbuf : Lexing.lexbuf) =
  match Parser.prog Lexer.read lexbuf with
  | x -> Ok x
  | exception Lexer.SyntaxError msg ->
      Error (SyntaxError (to_position lexbuf, msg))
  | exception Parser.Error -> Error (ParserError (to_position lexbuf))

let print_err outc { fname; ln; col } =
  Printf.fprintf outc "%s Ln %d, Col %d\n" fname ln col;
  let err_line = Io.read_nth_line_exn ln fname in
  let a, b = Print.arrow err_line ~index:(col - 1) in
  output_string outc (a ^ "\n");
  output_string outc (b ^ "\n");
  ()

let print_error outc = function
  | SyntaxError (pos, msg) ->
      output_string outc (msg ^ "\n");
      print_err outc pos
  | ParserError pos ->
      output_string outc "Parser error\n";
      print_err outc pos

(* let rec parse_and_print (lexbuf : Lexing.lexbuf) =
  match parse_line lexbuf with
  | Ok v -> (
      match v with
      | Some value ->
          print_endline (Ast.line_to_string value);
          parse_and_print lexbuf
      | None -> () )
  | Error e -> print_error e *)

let parse_lexbuf lexbuf =
  let rec aux xs =
    match parse_line lexbuf with
    | Ok v -> ( match v with Some x -> aux (x :: xs) | None -> Ok xs )
    | Error e -> Error e
  in
  match aux [] with Ok xs -> Ok (List.rev xs) | Error e -> Error e

let parse content ~name =
  let lexbuf = Lexing.from_string content in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
  parse_lexbuf lexbuf

let parse_file filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let parsed = parse_lexbuf lexbuf in
  close_in inx;
  parsed

let line_to_string = Print.line_to_string
