open Ds.Components;
let version = V.version;

module T = Rarg.Type;
module Cmd = Rarg.Cmd;
module Args = Rarg.Args;

type cmdResult = result(unit, string);

module StringSet = Set.Make(String);

module Display = {
  type t = Repo.Git.Display.t;

  let parser: T.t(t) = {
    name: "display",
    parse:
      fun
      | "name" => Ok(NameOnly)
      | "status" => Ok(NameStatus)
      | "stat" => Ok(Stat)
      | x => Error(Some(x ++ " is not a known display value.")),
    stringify:
      fun
      | NameOnly => "name"
      | NameStatus => "status"
      | Stat => "stat",
    choices:
      Some(T.Choices.HelpAndSuggestions([NameOnly, NameStatus, Stat])),
  };
  let arg = args =>
    Args.One.opt(
      ~args,
      ~name="--display",
      ~alias="-d",
      ~doc="How to display the result",
      parser,
    );
};

module CmdDiff = {
  let handle =
      (
        ~filter=?,
        ~comparedCommit=?,
        ~display: option(Display.t)=?,
        ~forkCommit: string,
        ~copy: bool,
        ~dash: list(string),
      ) => {
    let coloredDiffPromise =
      Repo.Git.diff(
        ~filter?,
        ~comparedCommit?,
        ~display?,
        ~color=true,
        ~dash,
        forkCommit,
      );
    let%lwt coloredDiff = coloredDiffPromise
    and mdDiff =
      copy
        ? Repo.Git.diff(
            ~filter?,
            ~comparedCommit?,
            ~display?,
            ~color=false,
            ~dash,
            forkCommit,
          )
        : coloredDiffPromise;
    let (toMd, toLines) = (
      () => GitPrint.Diff.toMd(mdDiff, ~display?, ()),
      () => GitPrint.Diff.toTerminal(coloredDiff, ~display?, ()),
    );
    let%lwt () = Printer.printAndCopy(~toMd, ~toLines, ~copy);
    Lwt.return(Ok());
  };
  let args = [];
  let (args, getBase) = GitArg.createBase(args);
  let (args, getCopy) = SharedArg.createCopy(args);
  let (args, getFilter) = GitArg.createFilter(args);
  let (args, getDash) =
    Args.Many.default(~args, ~name="--", ~doc="Files", ~default=[], T.string);
  let (args, getDisplay) = Display.arg(args);

  let run = map => {
    let%lwt result = GitArg.extractCommits(getBase(map));
    switch (result) {
    | Error(err) => Lwt.return(Error(err))
    | Ok((comparedCommit, forkCommit)) =>
      handle(
        ~forkCommit,
        ~comparedCommit?,
        ~display=?getDisplay(map),
        ~filter=?getFilter(map),
        ~copy=getCopy(map),
        ~dash=getDash(map),
      )
    };
  };
  let cmd: Cmd.t(Lwt.t(cmdResult)) =
    Cmd.make(
      ~name="Changed files",
      ~version,
      ~doc=
        <Lines>
          <Line marginBottom=1>
            "Get a diff excluding .xlf files (supports the same base-branch checked-branch format as owners changed)"
          </Line>
          <Line>
            "For example to copy a diff for pasting as a GitHub comment"
          </Line>
          <Line marginBottom=1 indent=2>
            <Span color=Cyan> "orwell git diff --filter M --copy" </Span>
          </Line>
          <Line> "Or to see the impacted files" </Line>
          <Line indent=2>
            <Span color=Cyan> "orwell git diff --display status" </Span>
          </Line>
        </Lines>,
      ~args,
      ~run,
      (),
    );
};

let args = [];
let run = _m =>
  Error(
    "You need to specify a subcommand, for example 'diff', enter --help for help.",
  )
  |> Lwt.return(_);
let cmd: Cmd.t(Lwt.t(cmdResult)) =
  Cmd.make(
    ~name="git",
    ~version,
    ~doc="Helpful git aliases",
    ~args,
    ~run,
    ~children=[("diff", CmdDiff.cmd)],
    (),
  );
