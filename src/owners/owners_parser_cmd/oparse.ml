module AP = DsSeed.AbsolutePath
module Args = Rarg.Args
module Type = Rarg.Type
module Cmd = Rarg.Cmd

module CmdParse = struct
  let args = []

  let args, get_filepath =
    let name = "owners-filepath" in
    let doc = "OWNERS Filepath to parse" in
    Args.Positional.One.req ~args ~name ~doc Type.string

  let handle ~filepath =
    let parse_result = Owners_parser.parse_file filepath in
    match parse_result with
    | Ok lines ->
        lines
        |> List.iter (fun line ->
               print_endline (Owners_parser.line_to_string line))
    | Error e -> Owners_parser.print_error stderr e

  let run m = handle ~filepath:(get_filepath m)

  let cmd =
    let doc = "Parse and display an owners file" in
    Cmd.make ~name:"Parse" ~doc ~version:"1.0" ~args ~run ()
end

module CmdFind = struct
  let args = []

  let args, get_filepaths =
    let doc = "Files to find the owners for" in
    Args.Positional.Many.req ~args ~name:"filepaths" ~doc Type.string

  let handle ~filepaths =
    let rootdir = Conf.read_rootdir () in
    let filepaths =
      filepaths |> List.map (fun path -> AP.make ~rootdir ~path)
    in
    let match_owners_results = Owners.matchOwners ~filepaths ~rootdir in
    match_owners_results
    |> List.iter (fun Owners.{ filepath; ownersFilepath; matchedOwners } ->
           Printf.printf "filepath=%s\n" (AP.toString filepath);
           Printf.printf "ownersFilepath=%s\n" (AP.toString ownersFilepath);
           print_endline
             (Owners.Print.PerFile.matchedOwnersToString matchedOwners);
           print_endline "-------------")

  let run m = handle ~filepaths:(get_filepaths m)

  let cmd =
    let name = "Find owners files and print them" in
    let doc = "Find, parse and print" in
    Cmd.make ~name ~doc ~version:"1.0" ~args ~run ()
end

module CmdOwners = struct
  let args = []

  let args, get_filepaths =
    let doc = "Files to find the owners for" in
    Args.Positional.Many.req ~args ~name:"filepaths" ~doc Type.string

  let handle ~filepaths =
    let rootdir = Conf.read_rootdir () in
    let filepaths =
      filepaths |> List.map (fun path -> AP.make ~rootdir ~path)
    in
    let match_owners_results =
      Owners.GroupedByOwnersFile.matchOwners ~filepaths ~rootdir
    in

    (* path: DsSeed.AbsolutePath.t,
    comments: list(string),
    noParent: bool,
    owners,
    perFileOwners: list((perFileFile, perFileOwners)), *)
    match_owners_results
    |> List.iter
         (fun Owners.GroupedByOwnersFile.
                { ownersFilepath; comments; noParent; owners; perFileOwners }
              ->
           Printf.printf "owners_path=%s\n" (AP.toString ownersFilepath);
           Printf.printf "no_parent=%b\n" noParent;
           Console.log "comments";
           Console.log comments;
           (* Console.log "owners.filepaths";
           Console.log owners.filepaths;
           Console.log "owners.owners";
           Console.log owners.owners; *)
           Console.log "---";
           (* Console.log "perFileOwners.filepaths";
           Console.log perFileOwners.filepaths;
           Console.log "perFileOwners.matches";
           Console.log
             (Owners.Print.PerFile.perFileListToString perFileOwners.matches); *)
           Console.log "\n\n")

  let run m = handle ~filepaths:(get_filepaths m)

  let cmd =
    let name = "Find owners files and print them" in
    let doc = "Find, parse and print" in
    Cmd.make ~name ~doc ~version:"1.0" ~args ~run ()
end

module CmdStart = struct
  let args = []

  let run _m =
    print_endline "Forgot to add a subcommand, enter --help for help."

  let children =
    [
      ("parse", CmdParse.cmd); ("find", CmdFind.cmd); ("owners", CmdOwners.cmd);
    ]

  let cmd = Cmd.make ~name:"Start" ~version:"1.0" ~args ~run ~children ()
end

let main =
  match Rarg.Run.autorun CmdStart.cmd with Ok _ -> exit 0 | Error _ -> exit 1
