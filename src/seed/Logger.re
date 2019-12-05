include Console;

let trace = (v, ~m) => {
  log(m);
  log(v);
  v;
};
