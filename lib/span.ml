type t = { source : Source.t; start : int; stop : int }

let make source ~start ~stop =
  if start < 0 || stop < start || stop > String.length (Source.contents source)
  then invalid_arg "Span.make";
  { source; start; stop }

let source t = t.source
let start t = t.start
let stop t = t.stop
