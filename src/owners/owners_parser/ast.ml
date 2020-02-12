type per_file_file = Filename of string | FileGlob of string

type per_file_owner = SetNoParent | Anyone | Email of string | Path of string

type line =
  | SetNoParent
  | Anyone
  | Comment of string
  | Email of string
  | Path of string
  | PerFile of per_file_file * per_file_owner
