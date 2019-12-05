let readOne = (cmd: string): Lwt.t(string) =>
  Lwt_process.shell(cmd) |> Lwt_process.pread_line(_);

let readAll = (cmd: string): Lwt.t(list(string)) => {
  Lwt_process.shell(cmd)
  |> Lwt_process.pread_lines(_)
  |> DsSeed.Stream.toList(_);
};

let writeOne = (cmd: string, ~content: string) =>
  Lwt_process.shell(cmd) |> Lwt_process.pwrite(_, content);

let readCurrentInChannel = ic => {
  switch (in_channel_length(ic)) {
  | 0 => Lwt.return([])
  | _ => Lwt_io.stdin |> Lwt_io.read_lines(_) |> DsSeed.Stream.toList(_)
  | exception _ =>
    Lwt_io.stdin |> Lwt_io.read_lines(_) |> DsSeed.Stream.toList(_)
  };
};
