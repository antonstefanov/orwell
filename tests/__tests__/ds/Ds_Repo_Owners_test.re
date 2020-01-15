open TestFramework;

module O = Repo.Owners;

describe("Ds_Repo", t => {
  let abs = path =>
    DsSeed.AbsolutePath.make(~rootdir=DsSeed.AbsolutePath.root, ~path);
  let formatEmail = x => x;
  let formatPath = DsSeed.AbsolutePath.toString;
  t.describe("Owners", t => {
    t.describe("PrintByOwnersFile", t => {
      let files: list(Owners.GroupedByOwnersFile.t) = [
        {
          ownersFilepath: abs("ownersFilepath1"),
          comments: ["comment-line11", "comment-line12"],
          noParent: true,
          owners:
            Some({
              filepaths: [abs("filepath11"), abs("filepath12")],
              owners: Emails(["email11", "email12"]),
            }),
          perFileOwners:
            Some({
              filepaths: [abs("filepath13"), abs("filepath14")],
              matches: [
                (
                  Filename("filename1"),
                  NoParentEmails(["email13", "email14"]),
                ),
                (FileGlob("filename11*"), Emails(["email15", "email16"])),
              ],
            }),
        },
        {
          ownersFilepath: abs("ownersFilepath2"),
          comments: ["comment-line21", "comment-line22"],
          noParent: true,
          owners:
            Some({
              filepaths: [abs("filepath21"), abs("filepath22")],
              owners: Anyone,
            }),
          perFileOwners:
            Some({
              filepaths: [abs("filepath23"), abs("filepath24")],
              matches: [
                (FileGlob("filename11*"), Emails(["email25", "email26"])),
                (Filename("filename2"), Anyone),
              ],
            }),
        },
        {
          ownersFilepath: abs("ownersFilepath3"),
          comments: [],
          noParent: false,
          owners: None,
          perFileOwners: None,
        },
      ];
      t.test("prints to md", t => {
        t.expect.string(
          OrwellCmds.OwnersPrint.ByOwnersFile.toMd(
            ~files,
            ~formatEmail,
            ~formatPath,
          ),
        ).
          toMatchSnapshot()
      });
      t.test("prints to terminal", t => {
        t.expect.lines(
          OrwellCmds.OwnersPrint.ByOwnersFile.toTerminal(
            ~files,
            ~formatEmail,
            ~formatPath,
          ),
        ).
          toMatchSnapshot()
      });
    })
  });
});
