## How it works

### owners_parser

- a lexer splits the owners file into tokens
- a parser then parses the file based on the tokens and builds an AST

### OwnersReader

- follows any owners files that contain references to other files
- converts the original ast to a type-safe representation

### Owners

- follows any per-file directives and ensures that every file has a correct owners file (for example there are cases of closest owners files that don't match the file)
- provides grouping utilities - for example from file-based list to owners-file-based list
