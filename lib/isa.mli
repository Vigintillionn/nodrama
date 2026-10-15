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

type 'n operand = { n : 'n; index : Index.t }
(** [n] is raw 4-digit operand field 0 to 9999 once assembled *)

type 'n instr =
  | HIA of Reg.t * Mode.t * 'n operand
  | OPT of Reg.t * Mode.t * 'n operand
  | AFT of Reg.t * Mode.t * 'n operand
  | VER of Reg.t * Mode.t * 'n operand
  | DEL of Reg.t * Mode.t * 'n operand
  | MOD of Reg.t * Mode.t * 'n operand
  | VGL of Reg.t * Mode.t * 'n operand
  | BIG of Reg.t * Target.t * 'n operand
  | SPR of Target.t * 'n operand
  | VSP of Cond.t * Target.t * 'n operand
  | SBR of Target.t * 'n operand
  | KTG
  | LEZ
  | DRU
  | DRS
  | NWL
  | NTS
  | STP

val pp :
  (Format.formatter -> 'n -> unit) -> Format.formatter -> 'n instr -> unit
(** Prints DRAMA source with the mode spelled out, e.g. [HIA.w R1, 0(R2+)]. *)
