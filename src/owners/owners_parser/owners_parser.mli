module Ast = Ast

type position = { fname : string; ln : int; col : int }

type parse_error = SyntaxError of position * string | ParserError of position

val print_error : out_channel -> parse_error -> unit

val parse : string -> name:string -> (Ast.line list, parse_error) result

val parse_file : string -> (Ast.line list, parse_error) result

val line_to_string : Ast.line -> string
