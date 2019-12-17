open Ds.Components;

module Args = Rarg.Args;
module T = Rarg.Type;

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

let createFilter = args =>
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

let createBase = args =>
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

let extractCommits = input =>
  switch (input) {
  | [] => Lwt.return(Error("Must provide a base"))
  | [base] =>
    let%lwt forkCommit = Repo.Git.getBaseCommit(base);
    Lwt.return(Ok((None, forkCommit)));
  | [base, comparedCommit] =>
    let%lwt forkCommit = Repo.Git.getBaseCommit(~comparedCommit, base);
    Lwt.return(Ok((Some(comparedCommit), forkCommit)));
  | _ => Error("Must provide 1 or 2") |> Lwt.return
  };
