let getClosestOwnersFilepath = (~absolutePath: string): option(string) => {
  switch (DsSeed.Fs.closestDir(~dir=absolutePath, ~filename="OWNERS")) {
  | None => None
  | Some(closestOwnersDir) =>
    Some(DsSeed.Path.join([closestOwnersDir, "OWNERS"]))
  };
};

module StringMap = Map.Make(String);
module StringSet = Set.Make(String);
module OwnersMap = {
  type fileOwnerMapping = {
    filepath: string,
    ownersFilePath: string,
  };
  type t = StringMap.t(list(string));
  let make = (filepaths: list(string)): t => {
    let allfiles: list(fileOwnerMapping) =
      List.map(
        filepath => {
          let ownersFilePath =
            DsSeed.Option.getExn(
              getClosestOwnersFilepath(~absolutePath=filepath),
              "owners make " ++ filepath,
            );
          {filepath, ownersFilePath};
        },
        filepaths,
      );
    List.fold_left(
      (map, {filepath, ownersFilePath}) =>
        switch (StringMap.find_opt(ownersFilePath, map)) {
        | None => StringMap.add(ownersFilePath, [filepath], map)
        | Some(filepaths) =>
          StringMap.add(ownersFilePath, [filepath, ...filepaths], map)
        },
      StringMap.empty,
      allfiles,
    );
  };
};

module OwnersLines = {
  type t = {
    ownersFilePath: string,
    filepaths: list(string),
    lines: list(string),
  };
  let fileToken = "file://";
  let rec readOwnersFileLines =
          (ownersFilePath, ~rootdir): Lwt.t(list(string)) => {
    let%lwt initialLines = DsSeed.Fs.readAllLines(ownersFilePath);
    let%lwt finalLines =
      Lwt_list.map_p(
        line =>
          if (DsSeed.Strings.startsWith(line, ~start=fileToken)) {
            let relativePath =
              DsSeed.Strings.substr(
                line,
                ~starti=String.length(fileToken),
                (),
              );
            readOwnersFileLines(
              DsSeed.Path.resolve([rootdir, relativePath]),
              ~rootdir,
            );
          } else {
            Lwt.return([line]);
          },
        initialLines,
      );
    List.fold_left((acc, lines) => lines @ acc, [], finalLines)
    |> Lwt.return(_);
  };
  let readOfMap = (map: OwnersMap.t, ~rootdir): Lwt.t(list(t)) => {
    let entries = StringMap.bindings(map);
    Lwt_list.map_p(
      ((ownersFilePath, filepaths)) => {
        let%lwt lines = readOwnersFileLines(ownersFilePath, ~rootdir);
        Lwt.return({ownersFilePath, filepaths, lines});
      },
      entries,
    );
  };
  let read = (filepaths: list(string), ~rootdir) =>
    OwnersMap.make(filepaths) |> readOfMap(_, ~rootdir);

  let readGroupedByOwnersFile =
      (~repodir: string, ~changedFiles: list(string), ~transform: t => t)
      : Lwt.t(list(t)) => {
    let cwd = Sys.getcwd();
    let changed2 =
      List.map(
        changedFile =>
          switch (changedFile.[0]) {
          | '.' => DsSeed.Path.resolve([cwd, changedFile])
          | '/' => changedFile
          | _ => DsSeed.Path.resolve([repodir, changedFile])
          },
        changedFiles,
      );
    let%lwt groupedOwners = read(changed2, ~rootdir=repodir);

    let repodirLength = String.length(repodir) + 1;
    let groupedOwnersWithoutRepo =
      List.map(
        ({lines, ownersFilePath, filepaths}) =>
          {
            lines,
            ownersFilePath:
              DsSeed.Strings.substr(
                ownersFilePath,
                ~starti=repodirLength,
                (),
              ),
            filepaths:
              List.map(
                fp => DsSeed.Strings.substr(fp, ~starti=repodirLength, ()),
                filepaths,
              ),
          }
          |> transform,
        groupedOwners,
      );
    Lwt.return(groupedOwnersWithoutRepo);
  };

  type paths = {
    ownersFilePaths: (string, int),
    filepaths: list(string),
  };
  type groupedByOwner = {
    owner: string,
    paths: list(paths),
  };

  let mergeUnique = (l1, l2) =>
    StringSet.(union(of_list(l1), of_list(l2)) |> elements);

  let groupByOwner = (owners: list(t)): list(groupedByOwner) => {
    List.fold_left(
      (map, {lines, ownersFilePath, filepaths}) => {
        let linesLength = List.length(lines);
        let visited = ref(StringSet.empty);
        List.fold_left(
          (ownerMap, owner) => {
            let key = owner ++ "##" ++ ownersFilePath;
            switch (StringSet.find_opt(key, visited^)) {
            | Some(_) => ownerMap
            | None =>
              visited := StringSet.add(key, visited^);
              switch (StringMap.find_opt(owner, ownerMap)) {
              | None =>
                StringMap.add(
                  owner,
                  {
                    owner,
                    paths: [
                      {
                        ownersFilePaths: (ownersFilePath, linesLength),
                        filepaths,
                      },
                    ],
                  },
                  ownerMap,
                )
              | Some({owner, paths}) =>
                StringMap.add(
                  owner,
                  {
                    owner,
                    paths: [
                      {
                        ownersFilePaths: (ownersFilePath, linesLength),
                        filepaths,
                      },
                      ...paths,
                    ],
                  },
                  ownerMap,
                )
              };
            };
          },
          map,
          lines,
        );
      },
      StringMap.empty,
      owners,
    )
    |> StringMap.bindings(_)
    |> List.map(((k, v)) => v, _);
  };
};
