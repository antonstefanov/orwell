open Ds.Components;
let version = V.version;

let createTransformOwners = repodir => {
  let contributorsFilepath =
    Repo.Contributors.getContributorsFilepath(repodir);

  let%lwt contributorsMap =
    Repo.Contributors.readFileToMap(contributorsFilepath);

  let transformContributors =
    Repo.Contributors.transformOwners(~contributorsMap);

  Lwt.return(transformContributors);
};

module T = Rarg.Type;
module Cmd = Rarg.Cmd;
module Args = Rarg.Args;

type cmdResult = result(unit, string);

module GroupBy = {
  type t =
    | OwnersFile
    | Owner;

  let parser: T.t(t) = {
    name: "group-by",
    parse:
      fun
      | "file" => Ok(OwnersFile)
      | "owner" => Ok(Owner)
      | x => Error(Some(x ++ " is not a known group-by value.")),
    stringify:
      fun
      | OwnersFile => "file"
      | Owner => "owner",
    choices: Some(T.Choices.HelpAndSuggestions([OwnersFile, Owner])),
  };
  let arg = args =>
    Args.One.default(
      ~args,
      ~name="--group-by",
      ~alias="-g",
      ~doc="How to group the owners",
      ~default=OwnersFile,
      parser,
    );
};
module StringSet = Set.Make(String);

let printAndCopy =
    (
      ~repodir: string,
      ~files: list(string),
      ~copy: bool,
      ~groupBy: GroupBy.t,
    ) => {
  let%lwt transformOwners = createTransformOwners(repodir);
  let%lwt groupedOwners =
    Repo.Owners.OwnersLines.readGroupedByOwnersFile(
      ~repodir,
      ~changedFiles=files,
      ~transform=transformOwners,
    );
  let (toMd, toLines) =
    switch (groupBy) {
    | OwnersFile =>
      let p = DeprecatedOwnersPrint.ByOwnersFile.printer;
      ((() => p.toMd(groupedOwners)), (() => p.toTerminal(groupedOwners)));
    | Owner =>
      let p = DeprecatedOwnersPrint.ByOwner.printer;
      let grouped = Repo.Owners.OwnersLines.groupByOwner(groupedOwners);
      ((() => p.toMd(grouped)), (() => p.toTerminal(grouped)));
    };

  let%lwt () = Printer.printAndCopy(~toMd, ~toLines, ~copy);
  Lwt.return(Ok());
};

module CmdOwnersFiles = {
  let handle =
      (
        ~files1: list(string),
        ~files2: list(string),
        ~copy: bool,
        ~groupBy: GroupBy.t,
      ) => {
    let%lwt repodir = Repo.Git.getRootDir();
    let%lwt files3 = Shell.Process.readCurrentInChannel(stdin);
    // piped files support
    switch (files1 @ files2 @ files3) {
    | [] => Lwt.return(Error("No files provided"))
    | files => printAndCopy(~files, ~repodir, ~copy, ~groupBy)
    };
  };
  let args = [];
  let (args, getFiles1) =
    Args.Positional.Many.default(
      ~args,
      ~name="files",
      ~doc="Files list to get owners",
      ~default=[],
      T.string,
    );
  let (args, getFiles2) =
    Args.Many.default(
      ~args,
      ~name="--",
      ~doc="Files list to get owners",
      ~default=[],
      T.string,
    );
  let (args, getGroupBy) = GroupBy.arg(args);
  let (args, getCopy) = SharedArg.createCopy(args);

  let run = map => {
    handle(
      ~files1=getFiles1(map),
      ~files2=getFiles2(map),
      ~copy=getCopy(map),
      ~groupBy=getGroupBy(map),
    );
  };

  let doc =
    <Lines>
      <Line> "Get owners based on a list of files" </Line>
      <Line marginBottom=2>
        "You can also pipe the output of other commands, for example"
      </Line>
      <Line indent=1>
        <Span color=Cyan>
          "find . -path node_modules -prune -o -type f -name '*.jpg' | orwell owners files"
        </Span>
      </Line>
    </Lines>;

  let cmd: Cmd.t(Lwt.t(cmdResult)) =
    Cmd.make(~name="Files", ~version, ~doc, ~args, ~run, ());
};

module CmdOwnersChanged = {
  let mergeUntracked = (existing: list(string)): Lwt.t(list(string)) => {
    let%lwt untracked = Repo.Git.getUntrackedChanges();
    List.fold_left(
      (set, filename) => StringSet.add(filename, set),
      StringSet.of_list(existing),
      untracked,
    )
    |> StringSet.elements(_)
    |> Lwt.return(_);
  };
  let handle =
      (
        ~filter=?,
        ~comparedCommit=?,
        ~forkCommit: string,
        ~copy: bool,
        ~groupBy: GroupBy.t,
        ~includeUntracked: bool,
      ) => {
    let%lwt repodir = Repo.Git.getRootDir();
    let%lwt changedFilesList =
      Repo.Git.getChangedFiles(~filter?, ~comparedCommit?, forkCommit);
    let%lwt changedFilesList =
      includeUntracked
        ? mergeUntracked(changedFilesList) : Lwt.return(changedFilesList);

    printAndCopy(~files=changedFilesList, ~repodir, ~copy, ~groupBy);
  };
  let args = [];
  let (args, getBase) = GitArg.createBase(args);
  let (args, getGroupBy) = GroupBy.arg(args);
  let (args, getCopy) = SharedArg.createCopy(args);
  let (args, getIncludeUntracked) =
    Args.One.boolFlag(
      ~args,
      ~name="--all",
      ~alias="-a",
      ~doc=
        "Whether to include all files (including untracked) in the changed files list",
      T.bool,
    );
  let (args, getFilter) = GitArg.createFilter(args);

  let run = map => {
    let%lwt result = GitArg.extractCommits(getBase(map));
    switch (result) {
    | Error(err) => Lwt.return(Error(err))
    | Ok((comparedCommit, forkCommit)) =>
      handle(
        ~forkCommit,
        ~comparedCommit?,
        ~filter=?getFilter(map),
        ~copy=getCopy(map),
        ~groupBy=getGroupBy(map),
        ~includeUntracked=getIncludeUntracked(map),
      )
    };
  };
  let doc =
    <Lines>
      <Line>
        "Get owners based on changes compared to a base branch or commit"
      </Line>
      <Line>
        "To view changed files owners of the current branch, compared to "
        <Span color=Yellow> "origin/master" </Span>
      </Line>
      <Line marginBottom=1 indent=2>
        <Span color=Cyan> "orwell owners changed --copy" </Span>
      </Line>
      <Line>
        "To view changed files owners between 2 branches/commits (useful when checking remote PR chains):"
      </Line>
      <Line indent=2>
        <Span color=Cyan>
          "orwell owners changed origin/base-branch origin/checked-branch"
        </Span>
      </Line>
    </Lines>;

  let cmd: Cmd.t(Lwt.t(cmdResult)) =
    Cmd.make(~name="Changed Files", ~version, ~doc, ~args, ~run, ());
};

let args = [];
let run = _m =>
  Error(
    "You need to specify a subcommand, for example 'changed', enter --help for help.",
  )
  |> Lwt.return(_);

let cmd: Cmd.t(Lwt.t(cmdResult)) = {
  let doc =
    <Lines>
      <Line> "Helpers related to the OWNERS files." </Line>
      <Line>
        <Span color=Yellow> "owners (cmd)" </Span>
        " is printing a human-readable list of owners in the terminal."
      </Line>
      <Line>
        "You can use "
        <Span color=Yellow> "--copy" </Span>
        " or "
        <Span color=Yellow> "-c" </Span>
        " to copy the output in "
        <Span color=Yellow> "Markdown" </Span>
        " format that you can paste in PRs."
      </Line>
    </Lines>;

  Cmd.make(
    ~name="Deprecated Owners - they don't support per-file directives",
    ~version,
    ~doc,
    ~args,
    ~run,
    ~children=[
      ("changed", CmdOwnersChanged.cmd),
      ("files", CmdOwnersFiles.cmd),
    ],
    (),
  );
};
