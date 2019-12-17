open Ds.Components;

module Args = Rarg.Args;
module T = Rarg.Type;

let createCopy = args =>
  Args.One.boolFlag(
    ~args,
    ~name="--copy",
    ~alias="-c",
    ~doc="Whether to copy the output to your clipboard",
    T.bool,
  );
