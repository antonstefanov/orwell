let toList = (stream: Lwt_stream.t('a)): Lwt.t(list('a)) =>
  Lwt_stream.fold((x, xs) => [x, ...xs], stream, []);
