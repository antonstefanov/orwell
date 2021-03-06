%{
  open Syntax

  let left x len = String.sub x len ((String.length x) - len)

  (* 5 = String.length "file:" *)
  let clean_path x = left x 5

  let clean_comment x = String.trim (left x 1)
%}

%token SET_NO_PARENT
%token PER_FILE
%token EQUALS
%token ASTERIX
%token <string> COMMENT
%token <string> EMAIL
%token <string> PATH
%token <string> FILENAME
%token <string> FILE_GLOB
%token EOF

%start <Ast.line option> prog
%type <Ast.per_file_file> per_file_file
%type <Ast.per_file_owner> per_file_owner
%%

prog:
  | v = line  { Some v }
  | EOF       { None   }

line:
  | ASTERIX                                   { Anyone                    }
  | SET_NO_PARENT                             { SetNoParent               }
  | x = COMMENT                               { Comment (clean_comment x) }
  | x = EMAIL                                 { Email x                   }
  | x = PATH                                  { Path (clean_path x)       }
  | PER_FILE; l = per_file_file; EQUALS; r = per_file_owner
                                              { PerFile (l, r)            }

per_file_file:
  | filename = FILENAME                       { Filename filename         }
  | file_glob = FILE_GLOB                     { FileGlob file_glob        }

per_file_owner:
  | ASTERIX                                   { Anyone                    }
  | email = EMAIL                             { Email email               }
  | path = PATH                               { Path (clean_path path)    }
  | SET_NO_PARENT                             { SetNoParent               }
