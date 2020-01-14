type t;

let root: t;

let make: (~rootdir: t, ~path: string) => t;

let map: (list(t), ~f: t => 'a) => list('a);

let toString: t => string;

let closestDir: (t, ~filename: string) => option(t);

let join: (t, ~filename: string) => t;

let unique: list(t) => list(t);

let compare: (t, t) => int;

let basename: t => string;

let dirname: t => t;
