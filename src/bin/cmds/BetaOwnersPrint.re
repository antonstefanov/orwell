module Printer = {
  module GroupedByOwnersFile = Owners.GroupedByOwnersFile;
  open Ds.Components;

  module ByOwnersFile = {
    let perFileFileToString = (perFileFile: Owners.perFileFile) =>
      switch (perFileFile) {
      | Filename(x) => "file: " ++ x
      | FileGlob(x) => "glob: " ++ x
      };
    let perFileToString =
        (
          (perFileFile, perFileOwners): (
            Owners.perFileFile,
            Owners.perFileOwners,
          ),
          ~formatEmail,
        ) => {
      let file = perFileFileToString(perFileFile);
      let owners =
        switch (perFileOwners) {
        | Anyone => "anyone"
        | NoParentEmails(emails) =>
          "no-parent: "
          ++ (List.map(formatEmail, emails) |> String.concat(", "))
        | Emails(emails) =>
          List.map(formatEmail, emails) |> String.concat(", ")
        };
      file ++ " | " ++ owners;
    };
    let ownersToString =
        (owners: Owners.GroupedByOwnersFile.owners, ~formatEmail) => {
      switch (owners) {
      | Anyone => "anyone"
      | Emails(emails) =>
        List.map(formatEmail, emails) |> String.concat(", ")
      };
    };
    let toMd =
        (
          ~files: list(GroupedByOwnersFile.t),
          ~formatPath: DsSeed.AbsolutePath.t => string,
          ~formatEmail: string => string,
        )
        : string =>
      List.map(
        (
          {
            GroupedByOwnersFile.ownersFilepath,
            comments,
            noParent,
            owners,
            perFileOwners,
          },
        ) =>
          <Lines>
            <Code> {formatPath(ownersFilepath)} </Code>
            {renderIfTrue(noParent, () => <Line> "no-parent" </Line>)}
            {renderIfMany(comments, () => <Quotes> ...comments </Quotes>)}
            {renderIfSome(perFileOwners, ({filepaths, matches}) =>
               <Lines>
                 <Lines>
                   ...{List.map(
                     perFile => perFileToString(perFile, ~formatEmail),
                     matches,
                   )}
                 </Lines>
                 <CodeBlock> ...{List.map(formatPath, filepaths)} </CodeBlock>
               </Lines>
             )}
            {renderIfSome(owners, ({filepaths, owners}) =>
               <Lines>
                 <Line> {ownersToString(owners, ~formatEmail)} </Line>
                 <CodeBlock> ...{List.map(formatPath, filepaths)} </CodeBlock>
               </Lines>
             )}
          </Lines>,
        files,
      )
      |> String.concat("\n---\n", _);

    let perFileOwnersToString =
        (perFileOwners: Owners.perFileOwners, ~formatEmail) =>
      switch (perFileOwners) {
      | Anyone => <Span color=Green> "anyone" </Span>
      | NoParentEmails(emails) =>
        <Span>
          <Span color=Blue> "no-parent " </Span>
          <Span color=Cyan>
            {List.map(formatEmail, emails) |> String.concat(", ")}
          </Span>
        </Span>
      | Emails(emails) =>
        <Span color=Cyan>
          {List.map(formatEmail, emails) |> String.concat(", ")}
        </Span>
      };
    let toTerminal =
        (
          ~files: list(GroupedByOwnersFile.t),
          ~formatPath: DsSeed.AbsolutePath.t => string,
          ~formatEmail: string => string,
        )
        : list(string) =>
      List.map(
        (
          {
            GroupedByOwnersFile.ownersFilepath,
            comments,
            noParent,
            owners,
            perFileOwners,
          },
        ) =>
          <Lines marginBottom=1>
            <Span color=White> {formatPath(ownersFilepath)} </Span>
            {renderIfTrue(noParent, () =>
               <Span color=Blue> "no-parent" </Span>
             )}
            {renderIfMany(comments, () =>
               <Span italic=true> {String.concat("\n", comments)} </Span>
             )}
            {renderIfSome(perFileOwners, ({filepaths, matches}) =>
               <Lines>
                 <Lines>
                   ...{List.map(
                     ((perFileFile, perFileOwners)) => {
                       <Span>
                         <Span bold=true>
                           {perFileFileToString(perFileFile)}
                         </Span>
                         " | "
                         {perFileOwnersToString(perFileOwners, ~formatEmail)}
                       </Span>
                     },
                     matches,
                   )}
                 </Lines>
                 <Lines indent=2>
                   ...{List.map(formatPath, filepaths)}
                 </Lines>
               </Lines>
             )}
            {renderIfSome(owners, ({filepaths, owners}) =>
               <Lines>
                 <Span color=Cyan>
                   {ownersToString(owners, ~formatEmail)}
                 </Span>
                 <Lines indent=2>
                   ...{List.map(formatPath, filepaths)}
                 </Lines>
               </Lines>
             )}
          </Lines>,
        files,
      );
  };
};
