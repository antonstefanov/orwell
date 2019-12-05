open TestFramework;

describe("CmdEntry", t => {
  t.test("validates command tree", t => {
    t.expect.result(Rarg.Cmd.validate(OrwellCmds.CmdEntry.CmdStart.cmd)).toBeOk();
  });
});
