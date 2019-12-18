let getBaseCommit =
    (~comparedCommit="HEAD", forkPoint: string): Lwt.t(string) =>
  Shell.Process.readOne(
    "git merge-base " ++ forkPoint ++ " " ++ comparedCommit,
  );

let getRootDir = (): Lwt.t(string) =>
  Shell.Process.readOne("git rev-parse --show-toplevel");

module Display = {
  type t =
    | NameOnly
    | NameStatus
    | Stat;
  let toString =
    fun
    | NameOnly => "--name-only"
    | NameStatus => "--name-status"
    | Stat => "--stat";
};

/*Internal*/
module I = {
  let ifTrue = (v, ~s) => v ? s : "";
  let map = (v, ~fn) => DsSeed.Option.bi(v, ~fn, ~default="");
  let opt = v => DsSeed.Option.getDefault(v, ~default="");
};

let diff =
    (
      ~filter=?,
      ~comparedCommit=?,
      ~dash=[],
      ~color=false,
      ~display: option(Display.t)=?,
      commit: string,
    )
    : Lwt.t(list(string)) =>
  Shell.Process.readAll(
    [
      "git diff",
      I.opt(Option.map(Display.toString, display)),
      I.map(filter, ~fn=f => "--diff-filter=" ++ f),
      commit,
      I.opt(comparedCommit),
      I.ifTrue(color, ~s="--color"),
      "--",
      "':!*.xlf'",
      ...dash,
    ]
    |> List.filter(v => v != "")
    |> String.concat(" ", _),
  );

let getChangedFiles =
    (~filter=?, ~comparedCommit=?, commit: string): Lwt.t(list(string)) =>
  diff(~filter?, ~comparedCommit?, ~color=false, ~display=NameOnly, commit);

let getUntrackedChanges = (): Lwt.t(list(string)) =>
  Shell.Process.readAll("git ls-files --others --exclude-standard");
