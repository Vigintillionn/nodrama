type severity = Error | Warning

type t = private {
  severity : severity;
  message : string;
  span : Span.t option;
  label : string option;
  notes : string list;
}

val error :
  ?span:Span.t ->
  ?label:string ->
  ?notes:string list ->
  ('a, Format.formatter, unit, t) format4 ->
  'a

val warning :
  ?span:Span.t ->
  ?label:string ->
  ?notes:string list ->
  ('a, Format.formatter, unit, t) format4 ->
  'a

val pp : Format.formatter -> t -> unit
