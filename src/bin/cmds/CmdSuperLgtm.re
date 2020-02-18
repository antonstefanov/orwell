open Ds.Components;
let version = V.version;

module T = Rarg.Type;
module Cmd = Rarg.Cmd;
module Args = Rarg.Args;

type cmdResult = result(unit, string);

let handle = () => {
  let%lwt repodir = Repo.Git.getRootDir();
  let rootdir = DsSeed.AbsolutePath.(make(~rootdir=root, ~path=repodir));
  let superLgtmPath =
    DsSeed.AbsolutePath.join(
      rootdir,
      ~filename="tools/check-lgtms/superlgtm",
    );
  let%lwt exitCode =
    Lwt_process.exec((
      "",
      [|"python", DsSeed.AbsolutePath.toString(superLgtmPath)|],
    ));
  let (s, code) =
    switch (exitCode) {
    | WEXITED(x) => ("WEXITED", x)
    | WSIGNALED(x) => ("WSIGNALED", x)
    | WSTOPPED(x) => ("WSTOPPED", x)
    };
  switch (code) {
  | 0 => Lwt.return(Ok())
  | x => Lwt.return(Error(Printf.sprintf("%s(%d)", s, x)))
  };
};
let args = [];

let run = map => handle();
let cmd: Cmd.t(Lwt.t(cmdResult)) =
  Cmd.make(
    ~name="superlgtm",
    ~version,
    ~doc=
      <Lines> <Line marginBottom=1> "An alias for superlgtm" </Line> </Lines>,
    ~args,
    ~run,
    (),
  );
