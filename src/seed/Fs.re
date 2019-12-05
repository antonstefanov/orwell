/**
 * Get the closest dir that contains a filename
 */
let rec closestDir = (~dir, ~filename): option(string) =>
  if (Sys.file_exists(Filename.concat(dir, filename))) {
    Some(dir);
  } else {
    switch (Filename.dirname(dir)) {
    // root reached
    | parentDir when parentDir == dir => None
    | parentDir => closestDir(~dir=parentDir, ~filename)
    };
  };

let readdir = path => Lwt_unix.readdir(path);

let dirname = path => Filename.dirname(path);

let read = (filename: string): Lwt_stream.t(string) =>
  Lwt_io.lines_of_file(filename);

let readAllLines = (filename: string): Lwt.t(list(string)) => {
  read(filename) |> Stream.toList(_);
};
