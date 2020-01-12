module AP = DsSeed.AbsolutePath;
module OwnersReader = OwnersReader;
module Ast = OwnersReader.OwnersAst;

type reason =
  | Anyone
  | PerFile(Ast.perFileFile)
  | Email;

type emails = {
  noParent: bool,
  emails: list(string),
};

type perFileFile =
  | Filename(string)
  | FileGlob(string);

let astToPerFileFile = (perFileFile: Ast.perFileFile): perFileFile => {
  switch (perFileFile) {
  | Filename(x) => Filename(x)
  | FileGlob(x) => FileGlob(x)
  };
};

type owners =
  | Anyone
  | Emails(emails)
  | PerFileAnyone(perFileFile)
  | PerFile(perFileFile, list(string))
  | Mixed({
      perFile: (perFileFile, list(string)),
      emails,
    });

type matchedOwners = {
  ownersFilepath: AP.t,
  owners,
  comments: list(string),
};

let checkPerFileMatch =
    (~filepath: AP.t, ~ownersFilepath: AP.t, ~perFiles: Ast.perFiles) => {
  let filename = AP.basename(filepath);
  let rec aux =
          (perFiles: Ast.perFiles)
          : option((Ast.perFileFile, Ast.perFileOwners)) => {
    switch (perFiles) {
    | [] => None
    | [(perFileFile, perFileOwners), ...rest] =>
      let match =
        switch (perFileFile) {
        | Filename(x) =>
          print_endline("Filename(" ++ x ++ "); filename=" ++ filename);
          print_endline("equals:" ++ string_of_bool(x == filename));
          x == filename ? Some((perFileFile, perFileOwners)) : None;
        | FileGlob(glob) =>
          print_endline("FileGlob(" ++ glob ++ "); filename=" ++ filename);
          let matches = DsSeed.Strings.matchesGlob(filename, ~glob);
          print_endline("equals:" ++ string_of_bool(matches));
          matches ? Some((perFileFile, perFileOwners)) : None;
        };
      switch (match) {
      | Some(x) => Some(x)
      | None => aux(rest)
      };
    };
  };
  aux(perFiles);
};

let globalEmailsOnly = (owners: Ast.owners) => {
  switch (owners) {
  | Anyone => None
  | PerFileOnly(perFiles) => None
  | EmailsOnly(emails) => Some(emails)
  | Mixed({emails}) => Some(emails)
  };
};

type checkMatchResult = {
  ownersFilepath: AP.t,
  ast: Ast.t,
  ownersPathAstMap: OwnersReader.OwnersPathAstMap.t,
  matchedOwners,
};

let rec checkMatch =
        (
          ~filepath: AP.t,
          ~ownersFilepath: AP.t,
          ~ast: Ast.t,
          ~ownersPathAstMap: OwnersReader.OwnersPathAstMap.t,
          ~rootdir: AP.t,
        )
        : matchedOwners => {
  let result =
    switch (ast.owners) {
    | Anyone => Anyone
    | PerFileOnly(perFiles) =>
      let matches = checkPerFileMatch(~filepath, ~ownersFilepath, ~perFiles);
      switch (matches) {
      | Some((perFileFile, perFileOwner)) =>
        let perFileFile = astToPerFileFile(perFileFile);
        switch (perFileOwner) {
        | Anyone => PerFileAnyone(perFileFile)
        | NoParentEmails(emails) => PerFile(perFileFile, emails)
        | Emails(emails) =>
          let parent = OwnersReader.closestOwnersFileExn(ownersFilepath);
          let (ast, ownersPathAstMap) =
            OwnersReader.maybeParseOwner(
              ~ownersPathAstMap,
              ~owners=parent,
              ~rootdir,
            );
          let globalEmails = globalEmailsOnly(ast.owners);
          switch (globalEmails) {
          | Some({noParent, emails as gemails}) =>
            Mixed({
              perFile: (perFileFile, emails),
              emails: {
                noParent,
                emails: gemails,
              },
            })
          | None => PerFile(perFileFile, emails)
          };
        };
      | None =>
        let parent = OwnersReader.closestOwnersFileExn(ownersFilepath);
        let (ast, ownersPathAstMap) =
          OwnersReader.maybeParseOwner(
            ~ownersPathAstMap,
            ~owners=parent,
            ~rootdir,
          );
        // wrong, should not return a part of it only
        checkMatch(
          ~filepath,
          ~ownersFilepath=parent,
          ~ast,
          ~ownersPathAstMap,
          ~rootdir,
        ).
          owners;
      };
    | EmailsOnly({noParent, emails}) => Emails({noParent, emails})
    | Mixed({perFiles, emails}) =>
      let matches = checkPerFileMatch(~filepath, ~ownersFilepath, ~perFiles);
      switch (matches) {
      | None => Emails({noParent: emails.noParent, emails: emails.emails})
      | Some((perFileFile, perFileOwner)) =>
        let perFileFile = astToPerFileFile(perFileFile);
        switch (perFileOwner) {
        | Anyone => PerFileAnyone(perFileFile)
        | NoParentEmails(emails) => PerFile(perFileFile, emails)
        | Emails(emails) =>
          let parent = OwnersReader.closestOwnersFileExn(ownersFilepath);
          let (ast, ownersPathAstMap) =
            OwnersReader.maybeParseOwner(
              ~ownersPathAstMap,
              ~owners=parent,
              ~rootdir,
            );
          let globalEmails = globalEmailsOnly(ast.owners);
          switch (globalEmails) {
          | Some({noParent, emails as gemails}) =>
            Mixed({
              perFile: (perFileFile, emails),
              emails: {
                noParent,
                emails: gemails,
              },
            })
          | None => PerFile(perFileFile, emails)
          };
        };
      };
    };
  {ownersFilepath, owners: result, comments: ast.comments};
};

let listToString = l => "[" ++ String.concat(", ", l) ++ "]";
let emailToString = ({noParent, emails}) =>
  "{ noParent:"
  ++ string_of_bool(noParent)
  ++ ", emails: "
  ++ listToString(emails)
  ++ " }";

let perFileToString =
  fun
  | Filename(x) => "Filename(" ++ x ++ ")"
  | FileGlob(x) => "FileGlob(" ++ x ++ ")";

let toString = owners => {
  switch (owners) {
  | Anyone => "Anyone"
  | Emails(emails) => "Emails(" ++ emailToString(emails) ++ ")"
  | PerFileAnyone(perFileFile) =>
    "PerFileAnyone(" ++ perFileToString(perFileFile) ++ ")"
  | PerFile(perFileFile, emails) =>
    "PerFile("
    ++ perFileToString(perFileFile)
    ++ ", "
    ++ listToString(emails)
    ++ ")"
  | Mixed({perFile, emails}) =>
    let (perFileFile, perFileEmails) = perFile;
    "Mixed("
    ++ "{ perFile: "
    ++ perFileToString(perFileFile)
    ++ ", perFileEmails: "
    ++ listToString(perFileEmails)
    ++ ", emails: "
    ++ emailToString(emails)
    ++ " })";
  };
};
