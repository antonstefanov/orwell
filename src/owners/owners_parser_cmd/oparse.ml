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

  let args = []

  let args, get_filepaths =
    let doc = "Files to find the owners for" in
    Args.Positional.Many.req ~args ~name:"filepaths" ~doc Type.string

  let handle ~filepaths =
    let rootdir =
      let str_dir = read_one_exn "git rev-parse --show-toplevel" in
      AP.make ~rootdir:AP.root ~path:str_dir
    in
    let files = filepaths |> List.map (fun path -> AP.make ~rootdir ~path) in
    let Owners.OwnersReader.{ closestOwnersMap; ownersPathAstMap } =
      Owners.OwnersReader.parseClosestOwners ~files ~rootdir
    in
    let owners =
      Owners.OwnersReader.FileOwnersPathMap.toList closestOwnersMap
    in
    owners
    |> List.iter (fun (filepath, ownersFilepath) ->
           let ast, ownersPathAstMap =
             Owners.OwnersReader.maybeParseOwner ~ownersPathAstMap
               ~owners:ownersFilepath ~rootdir
           in
           let Owners.{ ownersFilepath; owners; comments } =
             Owners.checkMatch ~filepath ~ownersFilepath ~ast ~ownersPathAstMap
               ~rootdir
           in
           Console.log "filepath";
           Console.log filepath;
           Console.log "ownersFilepath";
           Console.log ownersFilepath;
           Console.log "comments";
           Console.log comments;
           Console.log "owners";
           Console.log owners;
           Console.log "-------------";
           Console.log (Owners.toString owners))

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

  let children = [ ("parse", CmdParse.cmd); ("find", CmdFind.cmd) ]

  let cmd = Cmd.make ~name:"Start" ~version:"1.0" ~args ~run ~children ()
end

let main =
  match Rarg.Run.autorun CmdStart.cmd with Ok _ -> exit 0 | Error _ -> exit 1
