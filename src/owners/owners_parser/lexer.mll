{
  open Parser

  exception SyntaxError of string
}

let non_et = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '.']+
let email = non_et '@' non_et '.' non_et

let directory = "//"? ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '.' '~' '/']*
let file = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '.']+
let owner_file = "OWNERS" | file "_OWNERS"
let path = "file:" directory* owner_file
let file_extension = ['a'-'z' 'A'-'Z' '0'-'9']+
let filename = file '.' file_extension
let file_glob = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '.' '*']+

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = '#' [^'\n']*

rule read =
  parse
  | white                { read lexbuf                         }
  | newline              { Lexing.new_line lexbuf; read lexbuf }
  | "per-file"           { PER_FILE                            }
  | "set noparent"       { SET_NO_PARENT                       }
  | "="                  { EQUALS                              }
  | "*"                  { ASTERIX                             }
  | comment as x         { COMMENT x                           }
  | path as x            { PATH x                              }
  | email as x           { EMAIL x                             }
  | filename as x        { FILENAME x                          }
  | file_glob as x       { FILE_GLOB x                         }
  | eof                  { EOF                                 }
  | _ as x { raise (SyntaxError ("Unexpected char: " ^ (String.make 1 x))) }
