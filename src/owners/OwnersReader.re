module AP = DsSeed.AbsolutePath;
exception ParseError(Owners_parser.parse_error);
exception NoOwners;

module FileOwnersPathMap: {
  type t;
  let uniqueOwners: t => list(AP.t);
  let fromList: list((AP.t, AP.t)) => t;
  let toList: t => list((AP.t, AP.t));
  let findOwnersPath: (t, ~filepath: AP.t) => option(AP.t);
  /* let findOwnersPathExn: (t, ~filepath: AP.t) => AP.t; */
} = {
  module APMap = Map.Make(AP);
  type t = APMap.t(AP.t);
  type key = AP.t;
  let toList = APMap.bindings;
  let fromList = list =>
    List.fold_left(
      (acc, (key, value)) => APMap.add(key, value, acc),
      APMap.empty,
      list,
    );
  let values = map => map |> toList |> List.map(((_key, value)) => value);
  let uniqueOwners = (map: t) => map |> values |> AP.unique;
  let findOwnersPath = (map, ~filepath) => APMap.find_opt(filepath, map);
  /* let findOwnersPathExn = (map, ~filepath) =>
     findOwnersPath(map, ~filepath)
     |> DsSeed.Option.getExn(
          _,
          Printf.sprintf("filepath %s not found.", AP.toString(filepath)),
        ); */
};

let parseOwnersFile = (filepath: AP.t) => {
  switch (Owners_parser.parse_file(AP.toString(filepath))) {
  | Ok(x) => x
  // parse error are neither recoverable nor actionable
  | Error(e) => raise(ParseError(e))
  };
};

module FollowedOwnersAst = {
  type perFileFile =
    | Filename(string)
    | FileGlob(string);

  type anyoneOrEmail =
    | Anyone
    | Emails(list(string));

  type perFileOwner =
    | Anyone
    | SetNoParent
    | Email(string)
    | Remote(anyoneOrEmail);

  type line =
    | SetNoParent
    | Anyone
    | Comment(string)
    | Email(string)
    | PerFile(perFileFile, perFileOwner);

  let filterRemoteRelevant = (lines: list(line)) => {
    lines
    |> List.filter(
         fun
         // no parent is not relevant for remote files
         | SetNoParent => false
         | Anyone
         | Comment(_)
         | Email(_)
         | PerFile(_, _) => true,
       );
  };

  let astToPerFileFile =
      (perFileFile: Owners_parser.Ast.per_file_file): perFileFile => {
    switch (perFileFile) {
    | Filename(x) => Filename(x)
    | FileGlob(x) => FileGlob(x)
    };
  };

  let linesToPerFileRemote = (lines: list(line)) => {
    let rec aux = (lines, emails) => {
      switch (lines) {
      | [] => Emails(emails)
      | [x, ...xs] =>
        switch (x) {
        | Anyone => Anyone
        // set no-parent is not relevant for remote files
        | SetNoParent => aux(xs, emails)
        | Comment(x) => aux(xs, emails)
        | Email(x) => aux(xs, [x, ...emails])
        // per-file directives are not relevant for remote files
        | PerFile(_, _) => aux(xs, emails)
        }
      };
    };
    Remote(aux(lines, []));
  };

  let absPath = (~path, ~rootdir) => {
    let len = String.length("file:");
    let path = DsSeed.Strings.slice(path, ~starti=len, ());
    AP.make(~rootdir, ~path);
  };

  let rec follow = (lines: list(Owners_parser.Ast.line), ~rootdir) => {
    lines
    |> List.fold_left(
         (xs, x) => {
           switch (x) {
           | Owners_parser.Ast.SetNoParent => [SetNoParent, ...xs]
           | Comment(x) => [Comment(x), ...xs]
           | Email(x) => [Email(x), ...xs]
           | Path(path) =>
             let absolute = absPath(~rootdir, ~path);
             let parsed = parseOwnersFile(absolute);
             let followed = follow(parsed, ~rootdir);
             filterRemoteRelevant(followed) @ xs;
           | PerFile(perFileFile, perFileOwner) =>
             let left = astToPerFileFile(perFileFile);
             let right: perFileOwner =
               switch (perFileOwner) {
               | SetNoParent => SetNoParent
               | Anyone => Anyone
               | Email(x) => Email(x)
               | Path(path) =>
                 let absolute = absPath(~rootdir, ~path);
                 let parsed = parseOwnersFile(absolute);
                 let followed = follow(parsed, ~rootdir);
                 linesToPerFileRemote(followed);
               };
             [PerFile(left, right), ...xs];
           }
         },
         [],
       );
  };
};

module OwnersAst = {
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

  module PerFileMap: {
    type t;
    let empty: t;
    let add:
      (
        t,
        ~perFileFile: FollowedOwnersAst.perFileFile,
        ~perFileOwner: FollowedOwnersAst.perFileOwner
      ) =>
      t;
    let toPerFiles: t => perFiles;
  } = {
    module FileMap =
      Map.Make({
        type t = perFileFile;
        let compare = (a, b) => {
          switch (a, b) {
          | (Filename(_), FileGlob(_))
          | (FileGlob(_), Filename(_)) => (-1)
          | (Filename(a'), Filename(b')) => String.compare(a', b')
          | (FileGlob(a'), FileGlob(b')) => String.compare(a', b')
          };
        };
      });
    type value = {
      noParent: bool,
      anyone: bool,
      emails: list(string),
    };
    type t = FileMap.t(value);
    let empty = FileMap.empty;
    let astToPerFileFile =
        (perFileFile: FollowedOwnersAst.perFileFile): perFileFile => {
      switch (perFileFile) {
      | Filename(x) => Filename(x)
      | FileGlob(x) => FileGlob(x)
      };
    };
    let add =
        (
          map: t,
          ~perFileFile: FollowedOwnersAst.perFileFile,
          ~perFileOwner: FollowedOwnersAst.perFileOwner,
        ) => {
      let perFileFile = astToPerFileFile(perFileFile);
      let existing =
        switch (FileMap.find_opt(perFileFile, map)) {
        | Some(x) => x
        | None => {noParent: false, anyone: false, emails: []}
        };
      let value =
        switch (perFileOwner) {
        | Anyone => {...existing, anyone: true}
        | SetNoParent => {...existing, noParent: true}
        | Email(email) => {...existing, emails: [email, ...existing.emails]}
        | Remote(anyoneOrEmail) =>
          switch (anyoneOrEmail) {
          | Anyone => {...existing, anyone: true}
          | Emails(emails) => {...existing, emails: emails @ existing.emails}
          }
        };
      FileMap.add(perFileFile, value, map);
    };

    let toPerFiles = (map: t) => {
      let entries = FileMap.bindings(map);
      entries
      |> List.map(((perFileFile, {noParent, anyone, emails})) => {
           let perFileOwners: perFileOwners =
             anyone
               ? Anyone : noParent ? NoParentEmails(emails) : Emails(emails);
           (perFileFile, perFileOwners);
         });
    };
  };

  let ofFollowedLines = (lines: list(FollowedOwnersAst.line)): t => {
    let anyone = ref(false);
    let noParent = ref(false);
    let comments = ref([]);
    let emails = ref([]);
    let perFiles = ref(PerFileMap.empty);
    lines
    |> List.iter((line: FollowedOwnersAst.line) => {
         switch (line) {
         | Anyone => anyone := true
         | SetNoParent => noParent := true
         | Comment(x) => comments := [x, ...comments^]
         | Email(x) => emails := [x, ...emails^]
         | PerFile(perFileFile, perFileOwner) =>
           perFiles := PerFileMap.add(perFiles^, ~perFileFile, ~perFileOwner)
         }
       });
    let perFiles = PerFileMap.toPerFiles(perFiles^);

    let owners =
      if (anyone^) {
        Anyone;
      } else {
        switch (emails^, perFiles) {
        | ([], []) => raise(NoOwners)
        | ([], perFiles) => PerFileOnly(perFiles)
        | (emails, []) => EmailsOnly({noParent: noParent^, emails})
        | (emails, perFiles) =>
          Mixed({
            perFiles,
            emails: {
              noParent: noParent^,
              emails,
            },
          })
        };
      };

    {comments: comments^, owners};
  };
};

module OwnersPathAstMap: {
  type t;
  let add: (t, ~k: AP.t, ~v: OwnersAst.t) => t;
  let fromList: list((AP.t, OwnersAst.t)) => t;
  let findOwnersAst: (t, ~filepath: AP.t) => option(OwnersAst.t);
  let findOwnersAstExn: (t, ~filepath: AP.t) => OwnersAst.t;
} = {
  module APMap = Map.Make(AP);
  type t = APMap.t(OwnersAst.t);
  let add = (map, ~k, ~v) => APMap.add(k, v, map);
  let fromList = list =>
    List.fold_left((acc, (k, v)) => add(acc, ~k, ~v), APMap.empty, list);
  let findOwnersAst = (map, ~filepath) => APMap.find_opt(filepath, map);
  let findOwnersAstExn = (map, ~filepath) =>
    switch (findOwnersAst(map, ~filepath)) {
    | Some(x) => x
    | None =>
      failwith(
        Printf.sprintf(
          "No owners ast found for key %s",
          AP.toString(filepath),
        ),
      )
    };
  /* let toList = APMap.bindings;
     let values = map => map |> toList |> List.map(((_key, value)) => value);
     */
};

let closestOwnersFile = (dir: AP.t): option(AP.t) => {
  switch (AP.closestDir(dir, ~filename="OWNERS")) {
  | None => None
  | Some(closestOwnersDir) =>
    Some(AP.join(closestOwnersDir, ~filename="OWNERS"))
  };
};
let closestOwnersFileExn = (dir: AP.t) =>
  switch (closestOwnersFile(dir)) {
  | Some(x) => x
  | None =>
    failwith(
      Printf.sprintf("No owners file found near %s", AP.toString(dir)),
    )
  };
let findClosestOwnersFilepaths = (filepaths: list(AP.t)) => {
  let entries =
    AP.map(filepaths, ~f=filepath =>
      (filepath, closestOwnersFileExn(filepath))
    );
  FileOwnersPathMap.fromList(entries);
};

let maybeParseOwner =
    (~ownersPathAstMap: OwnersPathAstMap.t, ~owners: AP.t, ~rootdir: AP.t) => {
  switch (OwnersPathAstMap.findOwnersAst(ownersPathAstMap, ~filepath=owners)) {
  | Some(x) => (x, ownersPathAstMap)
  | None =>
    let lines = parseOwnersFile(owners);
    let followed = FollowedOwnersAst.follow(lines, ~rootdir);
    let x = OwnersAst.ofFollowedLines(followed);
    (x, OwnersPathAstMap.add(ownersPathAstMap, ~k=owners, ~v=x));
  };
};

/* let doA =
       (
         ~closestOwnersMap: FileOwnersPathMap.t,
         ~ownersPathAstMap: OwnersPathAstMap.t,
         ~filepath: AP.t,
         ~rootdir: AP.t,
       )
       : OwnersAst.t => {
     let closestOwners =
       switch (FileOwnersPathMap.findOwnersPath(closestOwnersMap, ~filepath)) {
       | Some(x) => x
       | None => closestOwnersFileExn(filepath)
       };
     switch (
       OwnersPathAstMap.findOwnersPath(ownersPathAstMap, ~filepath=closestOwners)
     ) {
     | Some(x) => x
     | None =>
       let lines = parseOwnersFile(closestOwners);
       let followed = FollowedOwnersAst.follow(lines, ~rootdir);
       OwnersAst.ofFollowedLines(followed);
     };
   }; */

type t = {
  closestOwnersMap: FileOwnersPathMap.t,
  ownersPathAstMap: OwnersPathAstMap.t,
};

let parseClosestOwners = (~files: list(AP.t), ~rootdir: AP.t): t => {
  let closestOwnersMap = findClosestOwnersFilepaths(files);
  let uniqueOwnersFiles = FileOwnersPathMap.uniqueOwners(closestOwnersMap);
  let rawOwners =
    uniqueOwnersFiles |> List.map(x => (x, parseOwnersFile(x)));
  let followedOwners =
    rawOwners
    |> List.map(((ownersPath, lines)) => {
         let followed = FollowedOwnersAst.follow(lines, ~rootdir);
         (ownersPath, OwnersAst.ofFollowedLines(followed));
       });
  {
    closestOwnersMap,
    ownersPathAstMap: OwnersPathAstMap.fromList(followedOwners),
  };
};
