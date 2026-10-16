val encode : int Isa.instr -> int
(** Raises [Invalid_argument] if an operand field is outside 0 to 9999 *)

val decode : int -> int Isa.instr option
(** [None] if the word is not a valid instruction; index regitster digit of an
    unindexed operand is ignored, so [encode] gives back the word with that
    digit cleared *)
