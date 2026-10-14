module Reg = struct
  type t = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9

  let to_int = function
    | R0 -> 0
    | R1 -> 1
    | R2 -> 2
    | R3 -> 3
    | R4 -> 4
    | R5 -> 5
    | R6 -> 6
    | R7 -> 7
    | R8 -> 8
    | R9 -> 9

  let of_int = function
    | 0 -> Some R0
    | 1 -> Some R1
    | 2 -> Some R2
    | 3 -> Some R3
    | 4 -> Some R4
    | 5 -> Some R5
    | 6 -> Some R6
    | 7 -> Some R7
    | 8 -> Some R8
    | 9 -> Some R9
    | _ -> None
end

module Mode = struct
  type t = Value | Address | Direct | Indirect
end

module Target = struct
  type t = Direct | Indirect
end

module Index = struct
  type t =
    | Unindexed
    | Indexed of Reg.t
    | Pre_inc of Reg.t
    | Post_inc of Reg.t
    | Pre_dec of Reg.t
    | Post_dec of Reg.t
end

module Cond = struct
  type t = NUL | NNUL | POS | NPOS | NEG | NNEG | OVL | GOVL | SO | GSO
end

type operand = { n : int; index : Index.t }

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
