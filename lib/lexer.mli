type kind =
  | Ident of string
  | Int of int
  | String of string
  | Comma
  | Semicolon
  | Colon
  | LParen
  | RParen
  | Plus
  | Minus
  | Star

type token = { kind : kind; span : Span.t }
type line = { tokens : token list; eol : Span.t }

val lines : Source.t -> (line, Diagnostic.t) result Seq.t
val pp_kind : Format.formatter -> kind -> unit
