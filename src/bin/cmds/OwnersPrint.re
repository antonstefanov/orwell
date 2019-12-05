open Ds.Components;

type printer('a) = {
  toMd: 'a => string,
  toTerminal: 'a => list(string),
};
module ByOwnersFile = {
  let toMd = (ownerlines: list(Repo.Owners.OwnersLines.t)): string =>
    List.map(
      ({Repo.Owners.OwnersLines.lines, ownersFilePath, filepaths}) =>
        <Lines>
          <Code> ownersFilePath </Code>
          <Line> {String.concat(", ", lines)} </Line>
          <CodeBlock> ...filepaths </CodeBlock>
        </Lines>,
      ownerlines,
    )
    |> String.concat("\n---\n", _);

  let toTerminal =
      (ownerlines: list(Repo.Owners.OwnersLines.t)): list(string) =>
    List.map(
      ({Repo.Owners.OwnersLines.lines, ownersFilePath, filepaths}) =>
        <Lines marginBottom=1>
          <Span color=White> ownersFilePath </Span>
          <Span color=Cyan> {String.concat(", ", lines)} </Span>
          <Lines indent=2> ...filepaths </Lines>
        </Lines>,
      ownerlines,
    );
  let printer: printer(list(Repo.Owners.OwnersLines.t)) = {
    toMd,
    toTerminal,
  };
};
module ByOwner = {
  module OwnersFilePath = {
    let createElement =
        (~children as _, ~item as (path, count): (string, int), ()) =>
      <Line>
        path
        {renderIfTrue(count > 1, () =>
           <Span> {" (shared by " ++ string_of_int(count) ++ ")"} </Span>
         )}
      </Line>;
  };
  module OwnersFilePaths = {
    let createElement = (~children as _, ~items: list((string, int)), ()) =>
      <Lines> ...{List.map(item => <OwnersFilePath item />, items)} </Lines>;
  };

  let toMd =
      (ownerlines: list(Repo.Owners.OwnersLines.groupedByOwner)): string =>
    List.map(
      ({Repo.Owners.OwnersLines.owner, paths}) =>
        <Lines>
          <Line> owner </Line>
          <Lines>
            ...{List.map(
              ({ownersFilePaths, filepaths}: Repo.Owners.OwnersLines.paths) =>
                <Lines marginBottom=1>
                  <OwnersFilePath item=ownersFilePaths />
                  <CodeBlock> ...filepaths </CodeBlock>
                </Lines>,
              paths,
            )}
          </Lines>
        </Lines>,
      ownerlines,
    )
    |> String.concat("\n---\n", _);

  let toTerminal =
      (ownerlines: list(Repo.Owners.OwnersLines.groupedByOwner))
      : list(string) =>
    List.map(
      ({Repo.Owners.OwnersLines.owner, paths}) =>
        <Lines marginBottom=1>
          <Span color=Cyan> owner </Span>
          <Lines>
            ...{List.map(
              ({ownersFilePaths, filepaths}: Repo.Owners.OwnersLines.paths) =>
                <Lines marginBottom=1>
                  <Span color=White>
                    <OwnersFilePath item=ownersFilePaths />
                  </Span>
                  <Lines indent=2> ...filepaths </Lines>
                </Lines>,
              paths,
            )}
          </Lines>
        </Lines>,
      ownerlines,
    );

  let printer: printer(list(Repo.Owners.OwnersLines.groupedByOwner)) = {
    toMd,
    toTerminal,
  };
};
