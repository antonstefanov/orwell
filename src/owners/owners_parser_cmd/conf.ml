module AP = DsSeed.AbsolutePath

let read_channel_once ic = try Some (input_line ic) with End_of_file -> None

let read_one cmd =
  let ic = Unix.open_process_in cmd in
  let res = read_channel_once ic in
  close_in_noerr ic;
  res

let read_one_exn cmd =
  match read_one cmd with
  | Some x -> x
  | None -> failwith ("no result from " ^ cmd)

let read_rootdir () =
  let path = read_one_exn "git rev-parse --show-toplevel" in
  AP.make ~rootdir:AP.root ~path
