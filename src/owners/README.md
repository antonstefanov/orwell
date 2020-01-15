## How it works

### owners_parser
https://github.com/antonstefanov/orwell/blob/master/src/owners/owners_parser/owners_parser.mli

- a [lexer](./owners_parser/lexer.mll) splits the owners file into tokens
- a [parser](./owners_parser/parser.mly) then takes the tokens, parses the file and builds an AST

The whole lexer is 29 loc: https://github.com/antonstefanov/orwell/blob/master/src/owners/owners_parser/lexer.mll#L7-L36
and the parser is another 20 loc: https://github.com/antonstefanov/orwell/blob/c1e643c61d9545ce1a0db0a80e9df232a856eac6/src/owners/owners_parser/parser.mly#L28-L48

### OwnersReader
https://github.com/antonstefanov/orwell/blob/c1e643c61d9545ce1a0db0a80e9df232a856eac6/src/owners/OwnersReader.rei#L43-L46
- follows any owners files that contain references to other files
- converts the original ast to a type-safe representation


### Owners
https://github.com/antonstefanov/orwell/blob/c1e643c61d9545ce1a0db0a80e9df232a856eac6/src/owners/Owners.rei#L56-L62
- follows any per-file directives and ensures that every file has a correct owners file (for example there are cases of closest owners files that don't match the file)
- provides grouping utilities - for example from file-based list to owners-file-based list
