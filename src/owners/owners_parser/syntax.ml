type owner_token =
  | SET_NO_PARENT
  | PER_FILE
  | HASH
  | STRING of string
  | EQUALS
  | EMAIL of string
  | PATH of string
  | FILENAME of string
  | FILE_GLOB of string
  | EOF
