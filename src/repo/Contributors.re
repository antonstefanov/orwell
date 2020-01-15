let getContributorsFilepath = (repodir: string) =>
  DsSeed.Path.resolve([repodir, "./tools/check-lgtms/CONTRIBUTORS"]);

type contributorsLine = {
  email: string,
  username: string,
};
let readFile = (filepath: string): Lwt.t(list(contributorsLine)) => {
  let stream = DsSeed.Fs.read(filepath);

  Lwt_stream.fold(
    (x, xs) => {
      let trimmedX = String.trim(x);
      switch (String.length(trimmedX)) {
      | 0 => xs
      | _ =>
        switch (trimmedX.[0]) {
        | '#' => xs
        | _ =>
          let (email, username) =
            DsSeed.Strings.splitAtChar(trimmedX, ~char=' ')
            |> DsSeed.Option.getExn(
                 _,
                 "invalid contributors line " ++ trimmedX,
               );
          [{email, username}, ...xs];
        }
      };
    },
    stream,
    [],
  );
};

module StringMap = Map.Make(String);

let toMap = (contributors: list(contributorsLine)): StringMap.t(string) =>
  List.fold_left(
    (map, {email, username}) => StringMap.add(email, username, map),
    StringMap.empty,
    contributors,
  );

let readFileToMap =
    (contributorsFilepath: string): Lwt.t(StringMap.t(string)) => {
  let%lwt contributorsList = readFile(contributorsFilepath);
  toMap(contributorsList) |> Lwt.return;
};

let emailToContributor = (email, ~contributorsMap) => {
  // remove '+' from emails my+something@email.com
  let email = Str.replace_first(Str.regexp(".+(\\+.+)@.+"), "", email);
  switch (StringMap.find_opt(email, contributorsMap)) {
  | Some(contributor) => "@" ++ contributor
  | None => email
  };
};

let transformOwners =
    (
      {lines, ownersFilePath, filepaths}: Owners.OwnersLines.t,
      ~contributorsMap: StringMap.t(string),
    )
    : Owners.OwnersLines.t => {
  ownersFilePath,
  filepaths,
  lines:
    List.map(
      line =>
        switch (StringMap.find_opt(line, contributorsMap)) {
        | Some(contributor) => "@" ++ contributor
        | None =>
          switch (DsSeed.Strings.splitAtChar(line, ~char='=')) {
          | None => line
          | Some((_, email)) =>
            switch (StringMap.find_opt(email, contributorsMap)) {
            | None => line
            | Some(contributor) => "@" ++ contributor
            }
          }
        },
      lines,
    ),
};
