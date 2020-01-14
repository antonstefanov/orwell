let read_nth_line_exn n filename =
  let ic = open_in filename in
  let rec aux i =
    match input_line ic with
    | line ->
        if i = n then (
          close_in ic;
          line )
        else aux (succ i)
    | exception End_of_file ->
        close_in ic;
        failwith "end of file reached"
  in
  aux 1
