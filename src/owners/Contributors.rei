module EmailUserMap: {
  type t;
  let findUserByEmail: (t, ~email: string) => option(string);
};
let readFileToMap: (~repodir: string) => Lwt.t(EmailUserMap.t);
