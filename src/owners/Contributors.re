type contributorsLine = {
  email: string,
  username: string,
};
module ContributorsFile: {
  type t;
  let ofRepodir: (~repodir: string) => t;
  let read: t => Lwt.t(list(contributorsLine));
} = {
  type t = string;
  let ofRepodir = (~repodir: string) =>
    DsSeed.Path.resolve([repodir, "./tools/check-lgtms/CONTRIBUTORS"]);
  let read = (filepath: t): Lwt.t(list(contributorsLine)) => {
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
            let maybeSplit = DsSeed.Strings.splitAtChar(trimmedX, ~char=' ');
            let (email, username) =
              DsSeed.Option.getExn(
                maybeSplit,
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
};

module EmailUserMap: {
  type t;
  let make: list(contributorsLine) => t;
  let findUserByEmail: (t, ~email: string) => option(string);
} = {
  module StringMap = Map.Make(String);
  type t = StringMap.t(string);
  let make = (contributors: list(contributorsLine)): t =>
    List.fold_left(
      (map, {email, username}) => StringMap.add(email, username, map),
      StringMap.empty,
      contributors,
    );
  let findUserByEmail = (map: t, ~email: string): option(string) => {
    switch (StringMap.find_opt(email, map)) {
    | Some(contributor) => Some("@" ++ contributor)
    | None => None
    };
  };
};

let readFileToMap = (~repodir: string): Lwt.t(EmailUserMap.t) => {
  let contributorsFilepath = ContributorsFile.ofRepodir(~repodir);
  let%lwt contributorsList = ContributorsFile.read(contributorsFilepath);
  Lwt.return(EmailUserMap.make(contributorsList));
};
