open Ds.Components;

module Diff = {
  let colorOfDiff = (char): Pastel.ColorName.colorName =>
    switch (char) {
    | 'A' => Green
    | 'C' => White
    | 'D' => Red
    | 'M' => Blue
    | 'R' => Yellow
    | 'T' => Yellow
    | 'U' => White
    | 'X' => White
    | 'B' => Magenta
    | '*' => BlackBright
    | _ => BlackBright
    };

  let toMd =
      (lines: list(string), ~display: option(Repo.Git.Display.t)=?, ())
      : string => {
    switch (display) {
    | None => <DiffBlock> {String.concat("\n", lines)} </DiffBlock>
    | Some(_) => <CodeBlock> {String.concat("\n", lines)} </CodeBlock>
    };
  };

  let toTerminal =
      (lines: list(string), ~display: option(Repo.Git.Display.t)=?, ())
      : list(string) =>
    switch (display) {
    | None => lines
    | Some(display) =>
      switch (display) {
      | NameOnly
      | Stat => lines
      | NameStatus =>
        List.map(
          line => {
            let (diffType, name) = Seed.Strings.splitAtIndex(line, ~index=1);
            <Lines>
              <Span>
                <Span color={colorOfDiff(diffType.[0])}> diffType </Span>
                " "
                <Span> name </Span>
              </Span>
            </Lines>;
          },
          lines,
        )
      }
    };
};
