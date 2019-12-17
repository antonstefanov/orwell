module Span =
  Pastel.Make({});

let isSome = DsSeed.Option.isSome;
let getMaxLength = items =>
  List.fold_left(
    (acc, text) =>
      switch (String.length(text)) {
      | len when len > acc => len
      | _ => acc
      },
    0,
    items,
  );
let leftRightLines =
    (items: list((string, string))): list((string, int, string)) => {
  let maxLength = getMaxLength(List.map(((left, _)) => left, items));
  List.map(
    ((left, right)) => (left, maxLength - String.length(left), right),
    items,
  );
};
let getLeftRightLine = (~startSpace, ~left, ~midSpace, ~right) => {
  let startSpace = DsSeed.Strings.repeat(" ", ~times=startSpace);
  let midSpace = DsSeed.Strings.repeat(" ", ~times=midSpace);
  <Span>
    startSpace
    <Span color=White> left </Span>
    midSpace
    <Span> right </Span>
  </Span>;
};
let spacedLeftRightLines = (items: list((string, int, string))) => {
  List.map(
    ((left, midSpace, right)) =>
      getLeftRightLine(~startSpace=2, ~left, ~midSpace=midSpace + 2, ~right),
    items,
  );
};
let tupleLines = (lines: list((string, string))): list(string) =>
  lines |> leftRightLines(_) |> spacedLeftRightLines(_);

let maybeIndent = (items: list(string), indent: int) =>
  indent == 0
    ? items
    : List.map(
        item => DsSeed.Strings.repeat(" ", ~times=indent) ++ item,
        items,
      );

let renderIfTrue = (cond: bool, render: unit => string) =>
  cond ? render() : "";

module Lines = {
  let createElement =
      (~children: list(string), ~indent=0, ~marginBottom=0, ()) =>
    String.concat("\n", maybeIndent(children, indent))
    ++ DsSeed.Strings.repeat("\n", ~times=marginBottom);
};
module Line = {
  let createElement =
      (~children: list(string), ~indent=0, ~marginBottom=0, ()) =>
    String.concat("", maybeIndent(children, indent))
    ++ DsSeed.Strings.repeat("\n", ~times=marginBottom);
};

module Code = {
  let createElement = (~children: list(string), ()) =>
    "`" ++ String.concat("", children) ++ "`";
};
module CodeBlock = {
  let createElement =
      (~children: list(string), ~lang=?, ~indent=0, ~marginBottom=0, ()) =>
    <Lines indent marginBottom>
      <Line>
        "```"
        <Span> {DsSeed.Option.getDefault(lang, ~default="")} </Span>
      </Line>
      <Lines> ...children </Lines>
      <Line> "```" </Line>
    </Lines>;
};

module DiffBlock = {
  let createElement =
      (~children: list(string), ~indent=0, ~marginBottom=0, ()) =>
    <Lines indent marginBottom>
      <Line> "````diff" </Line>
      <Lines> ...children </Lines>
      <Line> "````" </Line>
    </Lines>;
};
