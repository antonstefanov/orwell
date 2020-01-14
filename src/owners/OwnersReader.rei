module AP = DsSeed.AbsolutePath;
exception ParseError(Owners_parser.parse_error);
exception NoOwners;

module FileOwnersPathMap: {
  type t;
  let toList: t => list((AP.t, AP.t));
  // let findOwnersPath: (t, ~filepath: AP.t) => option(AP.t);
};
module OwnersAst: {
  type perFileFile =
    | Filename(string)
    | FileGlob(string);

  type perFileOwners =
    | Anyone
    | NoParentEmails(list(string))
    | Emails(list(string));

  type perFiles = list((perFileFile, perFileOwners));
  type emails = {
    noParent: bool,
    emails: list(string),
  };
  type owners =
    | Anyone
    | PerFileOnly(perFiles)
    | EmailsOnly(emails)
    | Mixed({
        perFiles,
        emails,
      });
  type t = {
    comments: list(string),
    owners,
  };
};
module OwnersPathAstMap: {
  type t;
  let findOwnersAst: (t, ~filepath: AP.t) => option(OwnersAst.t);
};

type t = {
  closestOwnersMap: FileOwnersPathMap.t,
  ownersPathAstMap: OwnersPathAstMap.t,
};

let closestOwnersFileExn: AP.t => AP.t;

let maybeParseOwner:
  (
    ~ownersPathAstMap: OwnersPathAstMap.t,
    ~ownersFilepath: AP.t,
    ~rootdir: AP.t
  ) =>
  (OwnersAst.t, OwnersPathAstMap.t);

let parseClosestOwners: (~filepaths: list(AP.t), ~rootdir: AP.t) => t;
