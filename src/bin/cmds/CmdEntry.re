module T = Rarg.Type;
module Cmd = Rarg.Cmd;
module Args = Rarg.Args;

type cmdResult = result(unit, string);

module CmdStart = {
  let args = [];

  let run = _m =>
    Error("Forgot to add a subcommand, enter --help for help.")
    |> Lwt.return(_);

  let cmd: Cmd.t(Lwt.t(cmdResult)) =
    Cmd.make(
      ~name="Start",
      ~version=V.version,
      ~args,
      ~run,
      ~children=[
        ("beta-owners", CmdBetaOwners.cmd),
        ("owners", CmdOwners.cmd),
        ("git", CmdGit.cmd),
      ],
      (),
    );
};
