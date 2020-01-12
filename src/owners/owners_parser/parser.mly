%{
  open Syntax
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
  | SET_NO_PARENT                             { SetNoParent         }
  | x = COMMENT                               { Comment x           }
  | x = EMAIL                                 { Email x             }
  | x = PATH                                  { Path x              }
  | PER_FILE; l = per_file_file; EQUALS; r = per_file_owner
                                              { PerFile (l, r)      }

per_file_file:
  | filename = FILENAME                       { Filename filename   }
  | file_glob = FILE_GLOB                     { FileGlob file_glob  }

per_file_owner:
  | ASTERIX                                   { Anyone              }
  | email = EMAIL                             { Email email         }
  | path = PATH                               { Path path           }
  | SET_NO_PARENT                             { SetNoParent         }
