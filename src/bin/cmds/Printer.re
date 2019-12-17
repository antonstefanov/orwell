open Ds.Components;

type t('a) = {
  toMd: 'a => string,
  toTerminal: 'a => list(string),
};

let printAndCopy = (~toLines, ~toMd, ~copy) => {
  let%lwt () = Lwt_list.iter_s(Lwt_io.printl, toLines());
  if (copy) {
    let content = toMd();
    let%lwt () = Shell.Process.writeOne("pbcopy", ~content);
    Lwt_io.printl(<Span color=Green> "Copied to your clipboard" </Span>);
  } else {
    Lwt.return();
  };
};
