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

module Arg = {
  let copy = args =>
    Args.One.boolFlag(
      ~args,
      ~name="--copy",
      ~alias="-c",
      ~doc="Whether to copy the output to your clipboard",
      T.bool,
    );
};

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
      let p = OwnersPrint.ByOwnersFile.printer;
      ((() => p.toMd(groupedOwners)), (() => p.toTerminal(groupedOwners)));
    | Owner =>
      let p = OwnersPrint.ByOwner.printer;
      let grouped = Repo.Owners.OwnersLines.groupByOwner(groupedOwners);
      ((() => p.toMd(grouped)), (() => p.toTerminal(grouped)));
    };

  let%lwt () = Lwt_list.iter_s(Lwt_io.printl, toLines());
  let%lwt () =
    if (copy) {
      let content = toMd();
      let%lwt () = Shell.Process.writeOne("pbcopy", ~content);
      Lwt_io.printl(<Span color=Green> "Copied to your clipboard" </Span>);
    } else {
      Lwt.return();
    };
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
  let (args, getCopy) = Arg.copy(args);
  let run = map => {
    handle(
      ~files1=getFiles1(map),
      ~files2=getFiles2(map),
      ~copy=getCopy(map),
      ~groupBy=getGroupBy(map),
    );
  };
  let cmd: Cmd.t(Lwt.t(cmdResult)) =
    Cmd.make(
      ~name="Files",
      ~version,
      ~doc="Get owners based on a list of files",
      ~args,
      ~run,
      (),
    );
};

module CmdOwnersChanged = {
  let fstr = (name, description) =>
    <Line indent=8>
      " "
      <Span bold=true> name </Span>
      <Span> description </Span>
    </Line>;
  let filters =
    [
      fstr("A", "Added"),
      fstr("C", "Copied"),
      fstr("D", "Deleted"),
      fstr("M", "Modified"),
      fstr("R", "Renamed"),
      fstr("T", "have their type (mode) changed"),
      fstr("U", "Unmerged"),
      fstr("X", "Unknown"),
      fstr("B", "have had their pairing Broken"),
      fstr("*", "All-or-none"),
    ]
    |> String.concat("\n");
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
  let (args, getBase) =
    Args.Positional.Many.default(
      ~args,
      ~name="base",
      ~doc=
        <Line>
          <Span>
            "Base branch or commit(s) to use for determining the changed files. For example "
          </Span>
          <Span bold=true> "origin/master" </Span>
          <Span> " or " </Span>
          <Span bold=true> "origin/master my-branch" </Span>
        </Line>,
      ~default=["origin/master"],
      T.branch,
    );
  let (args, getGroupBy) = GroupBy.arg(args);
  let (args, getCopy) = Arg.copy(args);
  let (args, getIncludeUntracked) =
    Args.One.boolFlag(
      ~args,
      ~name="--all",
      ~alias="-a",
      ~doc=
        "Whether to include all files (including untracked) in the changed files list",
      T.bool,
    );
  let (args, getFilter) =
    Args.One.opt(
      ~args,
      ~name="--filter",
      ~doc=
        <Lines>
          <Line>
            <Span> "How to filter changed files, for example " </Span>
            <Span bold=true> "ACD" </Span>
            <Span> ", use lower case to exclude." </Span>
          </Line>
          <Line> filters </Line>
        </Lines>,
      T.string,
    );
  let run = map => {
    let h = (~comparedCommit=?, forkCommit) =>
      handle(
        ~forkCommit,
        ~comparedCommit?,
        ~filter=?getFilter(map),
        ~copy=getCopy(map),
        ~groupBy=getGroupBy(map),
        ~includeUntracked=getIncludeUntracked(map),
      );
    switch (getBase(map)) {
    | [] => Error("Must provide a base") |> Lwt.return
    | [base] =>
      let%lwt forkCommit = Repo.Git.getBaseCommit(base);
      h(forkCommit);
    | [base, comparedCommit] =>
      let%lwt forkCommit = Repo.Git.getBaseCommit(~comparedCommit, base);
      h(~comparedCommit, forkCommit);
    | _ => Error("Must provide 1 or 2") |> Lwt.return
    };
  };
  let cmd: Cmd.t(Lwt.t(cmdResult)) =
    Cmd.make(
      ~name="Changed Files",
      ~version,
      ~doc="Get owners based on changes compared to a base branch or commit",
      ~args,
      ~run,
      (),
    );
};

let args = [];
let run = _m =>
  Error(
    "You need to specify a subcommand, for example 'changed', enter --help for help.",
  )
  |> Lwt.return(_);
let cmd: Cmd.t(Lwt.t(cmdResult)) =
  Cmd.make(
    ~name="Owners",
    ~version,
    ~doc="Helpers related to the OWNERS files.",
    ~args,
    ~run,
    ~children=[
      ("changed", CmdOwnersChanged.cmd),
      ("files", CmdOwnersFiles.cmd),
    ],
    (),
  );
