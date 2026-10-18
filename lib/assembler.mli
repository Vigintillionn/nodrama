type t

val memory_size : int
val assemble : Ast.line list -> (t, Diagnostic.t list) result

val size : t -> int
(** Number of words laid out from address 0, [RESGR] areas included *)

val word : t -> int -> int
(** [word t address] is the word at [address] as 10 digits read unsigned. Words
    past [size t] are 0 *)

val symbols : t -> (string * int) list
(** labels and their addresses sorted by name *)
