val parse :
  (Lexer.line, Diagnostic.t) result Seq.t ->
  (Ast.line list, Diagnostic.t list) result
(** Consumes lines up to and including the first [EINDPR] line *)
