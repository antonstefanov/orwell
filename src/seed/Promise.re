let await = Lwt.bind;
let catch = Lwt.catch;
let finally = Lwt.finalize;

let resolve = Lwt.return;
let reject = Lwt.fail;

let all = Lwt.join;
let race = Lwt.pick;
