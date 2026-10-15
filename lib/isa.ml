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

type 'n operand = { n : 'n; index : Index.t }

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

let pp_reg ppf r = Format.fprintf ppf "R%d" (Reg.to_int r)

let pp_mode ppf (mode : Mode.t) =
  Format.pp_print_string ppf
    (match mode with
    | Value -> ".w"
    | Address -> ".a"
    | Direct -> ".d"
    | Indirect -> ".i")

let pp_target ppf (target : Target.t) =
  Format.pp_print_string ppf
    (match target with Direct -> ".d" | Indirect -> ".i")

let pp_cond ppf (cond : Cond.t) =
  Format.pp_print_string ppf
    (match cond with
    | NUL -> "NUL"
    | NNUL -> "NNUL"
    | POS -> "POS"
    | NPOS -> "NPOS"
    | NEG -> "NEG"
    | NNEG -> "NNEG"
    | OVL -> "OVL"
    | GOVL -> "GOVL"
    | SO -> "SO"
    | GSO -> "GSO")

let pp_index ppf : Index.t -> unit = function
  | Unindexed -> ()
  | Indexed r -> Format.fprintf ppf "(%a)" pp_reg r
  | Pre_inc r -> Format.fprintf ppf "(+%a)" pp_reg r
  | Post_inc r -> Format.fprintf ppf "(%a+)" pp_reg r
  | Pre_dec r -> Format.fprintf ppf "(-%a)" pp_reg r
  | Post_dec r -> Format.fprintf ppf "(%a-)" pp_reg r

let pp pp_n ppf instr =
  let operand ppf { n; index } =
    Format.fprintf ppf "%a%a" pp_n n pp_index index
  in
  let class1 name r mode o =
    Format.fprintf ppf "%s%a %a, %a" name pp_mode mode pp_reg r operand o
  in
  match instr with
  | HIA (r, mode, o) -> class1 "HIA" r mode o
  | OPT (r, mode, o) -> class1 "OPT" r mode o
  | AFT (r, mode, o) -> class1 "AFT" r mode o
  | VER (r, mode, o) -> class1 "VER" r mode o
  | DEL (r, mode, o) -> class1 "DEL" r mode o
  | MOD (r, mode, o) -> class1 "MOD" r mode o
  | VGL (r, mode, o) -> class1 "VGL" r mode o
  | BIG (r, t, o) ->
      Format.fprintf ppf "BIG%a %a, %a" pp_target t pp_reg r operand o
  | SPR (t, o) -> Format.fprintf ppf "SPR%a %a" pp_target t operand o
  | SBR (t, o) -> Format.fprintf ppf "SBR%a %a" pp_target t operand o
  | VSP (c, t, o) ->
      Format.fprintf ppf "VSP%a %a, %a" pp_target t pp_cond c operand o
  | KTG -> Format.pp_print_string ppf "KTG"
  | LEZ -> Format.pp_print_string ppf "LEZ"
  | DRU -> Format.pp_print_string ppf "DRU"
  | DRS -> Format.pp_print_string ppf "DRS"
  | NWL -> Format.pp_print_string ppf "NWL"
  | NTS -> Format.pp_print_string ppf "NTS"
  | STP -> Format.pp_print_string ppf "STP"
