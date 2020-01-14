type emails = {
  noParent: bool,
  emails: list(string),
};

type perFileFile =
  | Filename(string)
  | FileGlob(string);

type perFileOwners =
  | Anyone
  | NoParentEmails(list(string))
  | Emails(list(string));

type matchedOwners =
  | Anyone
  | Emails(emails)
  | PerFileAnyone(perFileFile)
  | PerFile(perFileFile, list(string))
  | PerFileAndParents({
      perFile: (perFileFile, list(string)),
      parents: emails,
    });

type ownersConfig = {
  ownersFilepath: DsSeed.AbsolutePath.t,
  ast: OwnersReader.OwnersAst.t,
  ownersPathAstMap: OwnersReader.OwnersPathAstMap.t,
};

type matchOwnersResult = {
  filepath: DsSeed.AbsolutePath.t,
  ownersFilepath: DsSeed.AbsolutePath.t,
  matchedOwners,
  comments: list(string),
};
let matchOwners:
  (
    ~filepaths: list(DsSeed.AbsolutePath.t),
    ~rootdir: DsSeed.AbsolutePath.t
  ) =>
  list(matchOwnersResult);

module GroupedByOwnersFile: {
  type owners =
    | Anyone
    | Emails(list(string));
  type globalOwners = {
    filepaths: list(DsSeed.AbsolutePath.t),
    owners,
  };
  type perFiles = {
    filepaths: list(DsSeed.AbsolutePath.t),
    matches: list((perFileFile, perFileOwners)),
  };
  type t = {
    ownersFilepath: DsSeed.AbsolutePath.t,
    comments: list(string),
    noParent: bool,
    owners: option(globalOwners),
    perFileOwners: option(perFiles),
  };
  let matchOwners:
    (
      ~filepaths: list(DsSeed.AbsolutePath.t),
      ~rootdir: DsSeed.AbsolutePath.t
    ) =>
    list(t);
};

module Print: {
  module PerFile: {
    let perFileListToString: list((perFileFile, perFileOwners)) => string;
    let matchedOwnersToString: matchedOwners => string;
  };
};
