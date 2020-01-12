let slice (str : string) ~(starti : int) ?(endi : int option) () =
  let end_index = match endi with Some i -> i | None -> String.length str in
  String.sub str starti (end_index - starti)

let split_at_index (str : string) ~(index : int) : string * string =
  let left = slice str ~starti:0 ~endi:index () in
  let right = slice str ~starti:(index + 1) () in
  (left, right)

let repeat (str : string) ~(times : int) : string =
  let buff = Buffer.create (String.length str * times) in
  let _ =
    for _x = 1 to times do
      Buffer.add_string buff str
    done
  in
  Buffer.contents buff

let red s = "\x1b[31m" ^ s ^ "\x1b[39m"

let arrow (str : string) ~(index : int) : string * string =
  let len = String.length str in
  let str =
    if index < len - 1 then str else str ^ repeat " " ~times:(len - index + 1)
  in
  let left, right = split_at_index str ~index in
  let c = String.make 1 str.[index] in
  let line1 = left ^ red c ^ right in
  let line2 = repeat " " ~times:index ^ red "⤒" in
  (line1, line2)

let per_file_file_to_string = function
  | Ast.Filename x -> "Filename(" ^ x ^ ")"
  | FileGlob x -> "FileGlob(" ^ x ^ ")"

let per_file_owner_to_string = function
  | Ast.Anyone -> "Anyone"
  | SetNoParent -> "SetNoParent"
  | Email x -> "Email(" ^ x ^ ")"
  | Path x -> "Path(" ^ x ^ ")"

let line_to_string = function
  | Ast.SetNoParent -> "SetNoParent"
  | Comment x -> "Comment(" ^ x ^ ")"
  | Email x -> "Email(" ^ x ^ ")"
  | Path x -> "Path" ^ x ^ ")"
  | PerFile (per_file_file, per_file_owner) ->
      "PerFile("
      ^ per_file_file_to_string per_file_file
      ^ ", "
      ^ per_file_owner_to_string per_file_owner
      ^ ")"
