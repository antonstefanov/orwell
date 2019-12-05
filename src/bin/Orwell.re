open Ds.Components;
/**
You can define a logo to be displayed in --help
*/
let logo =
  <Span color=Magenta>
    {|
 ██████╗ ██████╗ ██╗    ██╗███████╗██╗     ██╗
██╔═══██╗██╔══██╗██║    ██║██╔════╝██║     ██║
██║   ██║██████╔╝██║ █╗ ██║█████╗  ██║     ██║
██║   ██║██╔══██╗██║███╗██║██╔══╝  ██║     ██║
╚██████╔╝██║  ██║╚███╔███╔╝███████╗███████╗███████╗
 ╚═════╝ ╚═╝  ╚═╝ ╚══╝╚══╝ ╚══════╝╚══════╝╚══════╝
|}
  </Span>;

// esy x orwell --help

let main =
  Lwt_main.run(
    {
      switch (Rarg.Run.autorun(~logo, OrwellCmds.CmdEntry.CmdStart.cmd)) {
      | Error(e) =>
        switch (e) {
        | ConfigError => exit(100)
        | UserError => exit(10)
        | UnknownError => exit(1)
        }
      | Ok(ok) =>
        switch (ok) {
        | Handled => Lwt.return()
        | Run(promise) =>
          let%lwt result = promise;
          switch (result) {
          | Ok(ok) => exit(0)
          | Error(e) =>
            let%lwt () = Lwt_io.printl(e);
            exit(1);
          };
        }
      };
    },
  );
