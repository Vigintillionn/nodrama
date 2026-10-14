module Reg : sig
  type t = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9

  val to_int : t -> int
  val of_int : int -> t option
end

module Mode : sig
  type t = Value | Address | Direct | Indirect
end

module Target : sig
  type t = Direct | Indirect
end

module Index : sig
  type t =
    | Unindexed
    | Indexed of Reg.t
    | Pre_inc of Reg.t
    | Post_inc of Reg.t
    | Pre_dec of Reg.t
    | Post_dec of Reg.t
end

module Cond : sig
  type t = NUL | NNUL | POS | NPOS | NEG | NNEG | OVL | GOVL | SO | GSO
end

type operand = { n : int; index : Index.t }
(** [n] is raw 4-digit operand field 0 to 9999 *)

type instr =
  | HIA of Reg.t * Mode.t * operand
  | OPT of Reg.t * Mode.t * operand
  | AFT of Reg.t * Mode.t * operand
  | VER of Reg.t * Mode.t * operand
  | DEL of Reg.t * Mode.t * operand
  | MOD of Reg.t * Mode.t * operand
  | VGL of Reg.t * Mode.t * operand
  | BIG of Reg.t * Target.t * operand
  | SPR of Target.t * operand
  | VSP of Cond.t * Target.t * operand
  | SBR of Target.t * operand
  | KTG
  | LEZ
  | DRU
  | DRS
  | NWL
  | NTS
  | STP
