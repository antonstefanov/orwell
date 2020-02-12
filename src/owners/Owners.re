module AP = DsSeed.AbsolutePath;
module OwnersReader = OwnersReader;
module Ast = OwnersReader.OwnersAst;

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

let astToPerFileFile = (perFileFile: Ast.perFileFile): perFileFile => {
  switch (perFileFile) {
  | Filename(x) => Filename(x)
  | FileGlob(x) => FileGlob(x)
  };
};

type matchedOwners =
  | Anyone
  | Emails(emails)
  | PerFileAnyone(perFileFile)
  | PerFile(perFileFile, list(string))
  | PerFileAndParents({
      perFile: (perFileFile, list(string)),
      parents: emails,
    });

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
          x == filename ? Some((perFileFile, perFileOwners)) : None
        | FileGlob(glob) =>
          let matches = DsSeed.Strings.matchesGlob(filename, ~glob);
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

type ownersConfig = {
  ownersFilepath: AP.t,
  ast: Ast.t,
  ownersPathAstMap: OwnersReader.OwnersPathAstMap.t,
};

type checkMatchResult = {
  ownersConfig,
  matchedOwners,
};

let rec checkMatch =
        (~filepath: AP.t, ~ownersConfig: ownersConfig, ~rootdir: AP.t)
        : checkMatchResult => {
  let res = {ownersConfig, matchedOwners: Anyone};
  let {ast, ownersFilepath, ownersPathAstMap} = ownersConfig;
  switch (ast.owners) {
  | Anyone => {...res, matchedOwners: Anyone}
  | PerFileOnly(perFiles) =>
    let matches = checkPerFileMatch(~filepath, ~ownersFilepath, ~perFiles);
    switch (matches) {
    | Some((perFileFile, perFileOwner)) =>
      let perFileFile = astToPerFileFile(perFileFile);
      switch (perFileOwner) {
      | Anyone => {...res, matchedOwners: PerFileAnyone(perFileFile)}
      | NoParentEmails(perFileEmails) => {
          ...res,
          matchedOwners: PerFile(perFileFile, perFileEmails),
        }
      | Emails(perFileEmails) =>
        let parent =
          OwnersReader.closestOwnersFileExn(
            ownersFilepath |> AP.dirname |> AP.dirname,
          );
        let (ast, ownersPathAstMap) =
          OwnersReader.maybeParseOwner(
            ~ownersPathAstMap,
            ~ownersFilepath=parent,
            ~rootdir,
          );
        let globalEmails = globalEmailsOnly(ast.owners);
        let matchedOwners =
          switch (globalEmails) {
          | None => PerFile(perFileFile, perFileEmails)
          | Some(globalEmails) =>
            PerFileAndParents({
              perFile: (perFileFile, perFileEmails),
              parents: {
                noParent: globalEmails.noParent,
                emails: globalEmails.emails,
              },
            })
          };
        let ownersConfig = {ownersFilepath: parent, ownersPathAstMap, ast};
        {matchedOwners, ownersConfig};
      };
    // no file matches -> move 1 level above
    | None =>
      let parent =
        OwnersReader.closestOwnersFileExn(
          ownersFilepath |> AP.dirname |> AP.dirname,
        );
      let (ast, ownersPathAstMap) =
        OwnersReader.maybeParseOwner(
          ~ownersPathAstMap,
          ~ownersFilepath=parent,
          ~rootdir,
        );
      let ownersConfig = {ownersFilepath: parent, ownersPathAstMap, ast};
      checkMatch(~filepath, ~ownersConfig, ~rootdir);
    };
  | EmailsOnly({noParent, emails}) => {
      ...res,
      matchedOwners: Emails({noParent, emails}),
    }
  | Mixed({perFiles, emails}) =>
    let matchedOwners = {
      let matches = checkPerFileMatch(~filepath, ~ownersFilepath, ~perFiles);
      switch (matches) {
      | None => Emails({noParent: emails.noParent, emails: emails.emails})
      | Some((perFileFile, perFileOwner)) =>
        let perFileFile = astToPerFileFile(perFileFile);
        switch (perFileOwner) {
        | Anyone => PerFileAnyone(perFileFile)
        | NoParentEmails(perFileEmails) =>
          PerFile(perFileFile, perFileEmails)
        | Emails(perFileEmails) =>
          PerFileAndParents({
            perFile: (perFileFile, perFileEmails),
            parents: {
              noParent: emails.noParent,
              emails: emails.emails,
            },
          })
        };
      };
    };
    {...res, matchedOwners};
  };
};

type matchOwnersResult = {
  filepath: AP.t,
  ownersFilepath: AP.t,
  matchedOwners,
  comments: list(string),
};
let matchOwners = (~filepaths: list(AP.t), ~rootdir: AP.t) => {
  let rec aux = (~acc, ~paths, ~ownersPathAstMap) => {
    switch (paths) {
    | [] => acc
    | [(filepath, ownersFilepath), ...paths] =>
      let (ast, ownersPathAstMap) =
        OwnersReader.maybeParseOwner(
          ~ownersPathAstMap,
          ~ownersFilepath,
          ~rootdir,
        );
      let ownersConfig = {ownersFilepath, ast, ownersPathAstMap};
      let {ownersConfig, matchedOwners} =
        checkMatch(~filepath, ~ownersConfig, ~rootdir);
      let {ownersFilepath, ast, ownersPathAstMap} = ownersConfig;
      let result = {
        filepath,
        ownersFilepath,
        matchedOwners,
        comments: ast.comments,
      };
      aux(~acc=[result, ...acc], ~paths, ~ownersPathAstMap);
    };
  };
  let OwnersReader.{closestOwnersMap, ownersPathAstMap} =
    OwnersReader.parseClosestOwners(~filepaths, ~rootdir);
  let paths = OwnersReader.FileOwnersPathMap.toList(closestOwnersMap);
  aux(~acc=[], ~paths, ~ownersPathAstMap);
};

module SS: {
  type t;
  let empty: t;
  let add: (t, string) => t;
  let addMany: (t, list(string)) => t;
  let toList: t => list(string);
} = {
  module StringSet = Set.Make(String);
  type t = StringSet.t;
  let empty = StringSet.empty;
  let add = (set, x) => StringSet.add(x, set);
  let addMany = (set, xs) => List.fold_left(add, set, xs);
  let toList = StringSet.elements;
};

module PerFileSet: {
  type t;
  type value = (perFileFile, perFileOwners);
  let empty: t;
  let add: (t, value) => t;
  let addMany: (t, list(value)) => t;
  let toList: t => list(value);
} = {
  type value = (perFileFile, perFileOwners);
  module S =
    Set.Make({
      type t = value;
      let compareLeft = (left_a, left_b) =>
        switch (left_a, left_b) {
        | (Filename(a), Filename(b)) => a == b
        | (FileGlob(a), FileGlob(b)) => a == b
        | (Filename(_), _) => false
        | (FileGlob(_), _) => false
        };

      let rec sameAux = (xs, ys) => {
        switch (xs, ys) {
        | ([], []) => true
        | ([x, ...xs], [y, ...ys]) => x == y && sameAux(xs, ys)
        | ([], _) => false
        | (_, []) => false
        };
      };
      let same = (xs, ys) => {
        List.length(xs) == List.length(ys) && sameAux(xs, ys);
      };

      let compare = ((left_a, right_a): t, (left_b, right_b): t) => {
        switch (compareLeft(left_a, left_b)) {
        | false => (-1)
        | true =>
          switch (right_a, right_b) {
          | (Anyone, Anyone) => 0
          | (NoParentEmails(xs), NoParentEmails(ys)) =>
            same(xs, ys) ? 0 : (-1)
          | (Emails(xs), Emails(ys)) => same(xs, ys) ? 0 : (-1)
          | (Anyone, _) => (-1)
          | (NoParentEmails(_), _) => (-1)
          | (Emails(_), _) => (-1)
          }
        };
      };
    });
  type t = S.t;
  let empty = S.empty;
  let add = (set, x) => S.add(x, set);
  let addMany = (set, xs) => List.fold_left(add, set, xs);
  let toList = S.elements;
};

module GroupedByOwnersFile: {
  type owners =
    | Anyone
    | Emails(list(string));
  type globalOwners = {
    filepaths: list(AP.t),
    owners,
  };
  type perFiles = {
    filepaths: list(AP.t),
    matches: list((perFileFile, perFileOwners)),
  };
  type t = {
    ownersFilepath: AP.t,
    comments: list(string),
    noParent: bool,
    owners: option(globalOwners),
    perFileOwners: option(perFiles),
  };
  let matchOwners: (~filepaths: list(AP.t), ~rootdir: AP.t) => list(t);
} = {
  // next steps: take all file-owners files
  // create a map: owner path <> ownersFile:
  type owners =
    | Anyone
    | Emails(list(string));
  type globalOwners = {
    filepaths: list(AP.t),
    owners,
  };
  type perFiles = {
    filepaths: list(AP.t),
    matches: list((perFileFile, perFileOwners)),
  };
  type t = {
    ownersFilepath: AP.t,
    comments: list(string),
    noParent: bool,
    owners: option(globalOwners),
    perFileOwners: option(perFiles),
  };

  type ownersFileGlobalOwners = {
    filepaths: list(AP.t),
    owners: SS.t,
  };
  type ownersFilePerFileOwners = {
    filepaths: list(AP.t),
    matches: PerFileSet.t,
  };
  type ownersFile = {
    comments: list(string),
    noParent: bool,
    anyone: bool,
    owners: ownersFileGlobalOwners,
    perFileOwners: ownersFilePerFileOwners,
  };
  module SM = Map.Make(AP);
  module APM: {
    type t;
    let empty: t;
    let add: (t, ~k: AP.t, ~v: ownersFile) => t;
    let findOpt: (t, AP.t) => option(ownersFile);
    let reduce:
      (t, ~initial: 'acc, ~f: ('acc, AP.t, ownersFile) => 'acc) => 'acc;
  } = {
    module APMap = Map.Make(AP);
    type t = APMap.t(ownersFile);
    let empty = APMap.empty;
    let add = (map, ~k, ~v) => APMap.add(k, v, map);
    let findOpt = (map, k) => APMap.find_opt(k, map);
    let reduce = (map, ~initial, ~f) => {
      APMap.fold((key, value, acc) => f(acc, key, value), map, initial);
    };
  };
  let groupMatchedOwners = (results: list(matchOwnersResult)) => {
    results
    |> List.fold_left(
         (acc, result: matchOwnersResult) => {
           let ownersFile =
             switch (APM.findOpt(acc, result.ownersFilepath)) {
             | Some(x) => x
             | None => {
                 comments: result.comments,
                 noParent: false,
                 anyone: false,
                 owners: {
                   filepaths: [],
                   owners: SS.empty,
                 },
                 perFileOwners: {
                   filepaths: [],
                   matches: PerFileSet.empty,
                 },
               }
             };

           let nextOwnersFile =
             switch (result.matchedOwners) {
             | Anyone => {
                 ...ownersFile,
                 anyone: true,
                 owners: {
                   filepaths: [
                     result.filepath,
                     ...ownersFile.owners.filepaths,
                   ],
                   owners: ownersFile.owners.owners,
                 },
               }
             | Emails({noParent, emails}) => {
                 ...ownersFile,
                 noParent,
                 owners: {
                   filepaths: [
                     result.filepath,
                     ...ownersFile.owners.filepaths,
                   ],
                   owners: SS.addMany(ownersFile.owners.owners, emails),
                 },
               }
             | PerFileAnyone(perFileFile) => {
                 ...ownersFile,
                 perFileOwners: {
                   filepaths: [
                     result.filepath,
                     ...ownersFile.perFileOwners.filepaths,
                   ],
                   matches:
                     PerFileSet.add(
                       ownersFile.perFileOwners.matches,
                       (perFileFile, Anyone),
                     ),
                 },
               }
             | PerFile(perFileFile, perFileEmails) => {
                 ...ownersFile,
                 perFileOwners: {
                   filepaths: [
                     result.filepath,
                     ...ownersFile.perFileOwners.filepaths,
                   ],
                   matches:
                     PerFileSet.add(
                       ownersFile.perFileOwners.matches,
                       (perFileFile, NoParentEmails(perFileEmails)),
                     ),
                 },
               }
             | PerFileAndParents({perFile, parents}) =>
               let (perFileFile, perFileOwners) = perFile;
               {
                 ...ownersFile,
                 noParent: parents.noParent,
                 owners: {
                   filepaths: [
                     result.filepath,
                     ...ownersFile.owners.filepaths,
                   ],
                   owners:
                     SS.addMany(ownersFile.owners.owners, parents.emails),
                 },
                 perFileOwners: {
                   filepaths: [
                     result.filepath,
                     ...ownersFile.perFileOwners.filepaths,
                   ],
                   matches:
                     PerFileSet.add(
                       ownersFile.perFileOwners.matches,
                       (perFileFile, Emails(perFileOwners)),
                     ),
                 },
               };
             };
           APM.add(acc, ~k=result.ownersFilepath, ~v=nextOwnersFile);
         },
         APM.empty,
       );
  };
  let mapToList = (map: APM.t) => {
    APM.reduce(
      map,
      ~initial=[],
      ~f=(
           acc,
           ownersFilepath,
           {comments, noParent, anyone, owners, perFileOwners},
         ) => {
        let xOwners: option(globalOwners) =
          switch (owners.filepaths) {
          | [] => None
          | _ =>
            Some({
              filepaths: owners.filepaths,
              owners: anyone ? Anyone : Emails(SS.toList(owners.owners)),
            })
          };
        let xPerFileOwners: option(perFiles) =
          switch (perFileOwners.filepaths) {
          | [] => None
          | _ =>
            Some({
              filepaths: perFileOwners.filepaths,
              matches: PerFileSet.toList(perFileOwners.matches),
            })
          };
        let x = {
          ownersFilepath,
          comments,
          noParent,
          owners: xOwners,
          perFileOwners: xPerFileOwners,
        };
        [x, ...acc];
      },
    );
  };

  let matchOwners = (~filepaths, ~rootdir) => {
    let matchedOwners = matchOwners(~filepaths, ~rootdir);
    let groupedOwners = groupMatchedOwners(matchedOwners);
    mapToList(groupedOwners);
  };
};

module Print = {
  let listToString = l => "[" ++ String.concat(", ", l) ++ "]";
  module PerFile = {
    let emailToString = ({noParent, emails}) =>
      "{ noParent:"
      ++ string_of_bool(noParent)
      ++ ", emails: "
      ++ listToString(emails)
      ++ " }";

    let perFileFileToString =
      fun
      | Filename(x) => "Filename(" ++ x ++ ")"
      | FileGlob(x) => "FileGlob(" ++ x ++ ")";

    let perFileOwnersToString = (perFileOwners: perFileOwners) =>
      switch (perFileOwners) {
      | Anyone => "Anyone"
      | NoParentEmails(xs) => "NoParentEmails(" ++ listToString(xs) ++ ")"
      | Emails(xs) => "Emails(" ++ listToString(xs) ++ ")"
      };

    let perFileListToString = xs =>
      xs
      |> List.map(((perFileFile, perFileOwners)) => {
           "( "
           ++ perFileFileToString(perFileFile)
           ++ ", "
           ++ perFileOwnersToString(perFileOwners)
           ++ " )"
         })
      |> listToString;

    let matchedOwnersToString = owners => {
      switch (owners) {
      | Anyone => "Anyone"
      | Emails(emails) => "Emails(" ++ emailToString(emails) ++ ")"
      | PerFileAnyone(perFileFile) =>
        "PerFileAnyone(" ++ perFileFileToString(perFileFile) ++ ")"
      | PerFile(perFileFile, emails) =>
        "PerFile("
        ++ perFileFileToString(perFileFile)
        ++ ", "
        ++ listToString(emails)
        ++ ")"
      | PerFileAndParents({perFile, parents}) =>
        let (perFileFile, perFileEmails) = perFile;
        "PerFileAndParents("
        ++ "{ perFile: "
        ++ perFileFileToString(perFileFile)
        ++ ", perFileEmails: "
        ++ listToString(perFileEmails)
        ++ ", parents: "
        ++ emailToString(parents)
        ++ " })";
      };
    };
  };
};
