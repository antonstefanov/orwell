open TestFramework;

module O = Repo.Owners;
describe("Ds_Repo", t => {
  t.describe("DeprecatedOwners", t => {
    t.describe("PrintByOwnersFile", t => {
      let ownersLines: list(Repo.Owners.OwnersLines.t) = [
        {
          ownersFilePath: "ownersFilePath1",
          filepaths: ["filepath11", "filepath12"],
          lines: ["owner11", "owner12"],
        },
        {
          ownersFilePath: "ownersFilePath2",
          filepaths: ["filepath21", "filepath22"],
          lines: ["owner21", "owner22"],
        },
      ];
      t.test("prints to md", t => {
        t.expect.string(
          OrwellCmds.DeprecatedOwnersPrint.ByOwnersFile.toMd(ownersLines),
        ).
          toMatchSnapshot()
      });
      t.test("prints to terminal", t => {
        t.expect.lines(
          OrwellCmds.DeprecatedOwnersPrint.ByOwnersFile.toTerminal(
            ownersLines,
          ),
        ).
          toMatchSnapshot()
      });
    });
    t.describe("PrintByOwners", t => {
      let ownersLines: list(Repo.Owners.OwnersLines.groupedByOwner) = [
        {
          owner: "owner1",
          paths: [
            {
              ownersFilePaths: ("ownersFilePaths11", 1),
              filepaths: ["filepath11"],
            },
            {
              ownersFilePaths: ("ownersFilePaths12", 2),
              filepaths: ["filepath12"],
            },
          ],
        },
        {
          owner: "owner2",
          paths: [
            {
              ownersFilePaths: ("ownersFilePaths21", 2),
              filepaths: ["filepath21"],
            },
            {
              ownersFilePaths: ("ownersFilePaths22", 1),
              filepaths: ["filepath22"],
            },
          ],
        },
      ];
      t.test("prints to md", t => {
        t.expect.string(
          OrwellCmds.DeprecatedOwnersPrint.ByOwner.toMd(ownersLines),
        ).
          toMatchSnapshot()
      });
      t.test("prints to terminal", t => {
        t.expect.lines(
          OrwellCmds.DeprecatedOwnersPrint.ByOwner.toTerminal(ownersLines),
        ).
          toMatchSnapshot()
      });
    });
  })
});
