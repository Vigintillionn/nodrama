type ident = { name : string; span : Span.t }
(** [name] is folded to upper case *)
type sign = Add | Sub
type expr = { const : int; symbol : (sign * ident) option; span : Span.t }
(** [const + symbol] or [const - symbol] *)

type item = Word of expr | Text of string

type statement =
  | Instr of expr Isa.instr
  | Data of item list
  | Resgr of int
  | Eindpr

type line = {
  label : ident option;
  statement : statement option;
  span : Span.t;
}
