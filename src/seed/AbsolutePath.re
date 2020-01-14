type t = string;

let root = "/";

let map = (xs: list(t), ~f: t => 'a) => List.map(f, xs);

let toString = (x: t) => x;

let closestDir = (dir: t, ~filename: string) => {
  Fs.closestDir(~dir=toString(dir), ~filename);
};
let join = (dir, ~filename: string) => Path.join([dir, filename]);

let dirname = path => Filename.dirname(path);

let unique = (dirs: list(t)): list(t) =>
  List.sort_uniq(String.compare, dirs);

let compare = (a, b) => String.compare(a, b);

let basename = path => Filename.basename(path);

let make = (~rootdir: t, ~path: string): t => {
  let cwd = Sys.getcwd();
  switch (path.[0]) {
  | '.' => Path.resolve([cwd, path])
  | '/' =>
    if (String.length(path) === 0) {
      "/";
    } else {
      // consider // to be "relative to rootdir"
      switch (path.[1]) {
      | '/' => Path.join([rootdir, path])
      | _ => path
      };
    }
  | _ => Path.resolve([rootdir, path])
  };
};
