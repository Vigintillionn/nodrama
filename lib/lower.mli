val is_code : Assembler.t -> int -> bool
(** Whether the word at an address is a valid instruction at compile time, ie
    whether it gets compiled code *)

val instr :
  is_code:(int -> bool) -> addr:int -> int Isa.instr -> Riscv.instr list
(** Lowering of one instruction at DRAMA address [addr] without its label *)

val program : Assembler.t -> Riscv.instr list
(** Every code word in address order, each under its [Word] label. Wherehever
    exection falls through to a word that has no code, including at start
    (address 0) and past address 9999 a jump is inserted *)
