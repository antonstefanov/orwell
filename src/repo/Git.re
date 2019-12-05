let getBaseCommit =
    (~comparedCommit="HEAD", forkPoint: string): Lwt.t(string) =>
  Shell.Process.readOne(
    "git merge-base " ++ forkPoint ++ " " ++ comparedCommit,
  );

let getRootDir = (): Lwt.t(string) =>
  Shell.Process.readOne("git rev-parse --show-toplevel");

let getChangedFiles =
    (~filter=?, ~comparedCommit=?, commit: string): Lwt.t(list(string)) =>
  Shell.Process.readAll(
    "git diff --name-only "
    ++ DsSeed.Option.bi(
         filter,
         ~fn=f => "--diff-filter=" ++ f ++ " ",
         ~default="",
       )
    ++ commit
    ++ " "
    ++ DsSeed.Option.getDefault(comparedCommit, ~default=""),
  );

let getUntrackedChanges = (): Lwt.t(list(string)) =>
  Shell.Process.readAll("git ls-files --others --exclude-standard");
