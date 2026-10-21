type reg =
  | Zero
  | S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | T0
  | T1
  | T2
  | A0

type label = Word of int | Skip of int | BadJump
type cmp = Eq | Ne | Lt | Ge | Ltu | Geu
type routine = Lez | Dru | Drs | Nwl | Stp
type resume = At of int | In of reg

type instr =
  | Label of label
  | Comment of string
  | Li of reg * int
  | Mv of reg * reg
  | Addi of reg * reg * int
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Div of reg * reg * reg
  | Ld of reg * int * reg
  | Sd of reg * int * reg
  | Ld_word of reg * int
  | Sd_word of reg * int * reg
  | Lla_word of reg * int
  | B of cmp * reg * reg * label
  | J of label
  | Call of routine
  | Enter_interp
  | Wrap10 of reg
  | Wrap10_ovi of reg
  | Ea
  | Dispatch
  | Mul10 of reg * reg
  | Divchk of reg * int
  | Absmod of reg * reg
  | Cstore of reg * resume

let negate = function
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Ge -> Lt
  | Ltu -> Geu
  | Geu -> Ltu

let fits_imm n = -2048 <= n && n <= 2047

let reg_name = function
  | Zero -> "zero"
  | S0 -> "s0"
  | S1 -> "s1"
  | S2 -> "s2"
  | S3 -> "s3"
  | S4 -> "s4"
  | S5 -> "s5"
  | S6 -> "s6"
  | S7 -> "s7"
  | S8 -> "s8"
  | S9 -> "s9"
  | S10 -> "s10"
  | S11 -> "s11"
  | T0 -> "t0"
  | T1 -> "t1"
  | T2 -> "t2"
  | A0 -> "a0"

let cmp_name = function
  | Eq -> "beq"
  | Ne -> "bne"
  | Lt -> "blt"
  | Ge -> "bge"
  | Ltu -> "bltu"
  | Geu -> "bgeu"

let routine_name = function
  | Lez -> "__drama_lez"
  | Dru -> "__drama_dru"
  | Drs -> "__drama_drs"
  | Nwl -> "__drama_nwl"
  | Stp -> "__drama_stp"

let pp_reg ppf r = Format.pp_print_string ppf (reg_name r)

let pp_label ppf = function
  | Word a -> Format.fprintf ppf ".Lw%d" a
  | Skip a -> Format.fprintf ppf ".Ls%d" a
  | BadJump -> Format.pp_print_string ppf ".Lbadjump"

let pp_word ppf a = Format.fprintf ppf "drama_mem+%d" (8 * a)

let pp ppf instr =
  let op fmt = Format.fprintf ppf (" " ^^ fmt) in
  match instr with
  | Label l -> Format.fprintf ppf "%a:" pp_label l
  | Comment s -> op "# %s" s
  | Li (rd, n) -> op "li %a, %d" pp_reg rd n
  | Mv (rd, rs) -> op "mv %a, %a" pp_reg rd pp_reg rs
  | Addi (rd, rs, n) -> op "addi %a, %a, %d" pp_reg rd pp_reg rs n
  | Add (rd, a, b) -> op "add %a, %a, %a" pp_reg rd pp_reg a pp_reg b
  | Sub (rd, a, b) -> op "sub %a, %a, %a" pp_reg rd pp_reg a pp_reg b
  | Div (rd, a, b) -> op "div %a, %a, %a" pp_reg rd pp_reg a pp_reg b
  | Ld (rd, off, base) -> op "ld %a, %d(%a)" pp_reg rd off pp_reg base
  | Sd (rs, off, base) -> op "sd %a, %d(%a)" pp_reg rs off pp_reg base
  | Ld_word (rd, a) -> op "ld %a, %a" pp_reg rd pp_word a
  | Sd_word (rs, a, tmp) -> op "sd %a, %a, %a" pp_reg rs pp_word a pp_reg tmp
  | Lla_word (rd, a) -> op "lla %a, %a" pp_reg rd pp_word a
  | B (cmp, a, b, l) ->
      op "%s %a, %a, %a" (cmp_name cmp) pp_reg a pp_reg b pp_label l
  | J l -> op "j %a" pp_label l
  | Call r -> op "call %s" (routine_name r)
  | Enter_interp -> op "tail __drama_enter_interp"
  | Wrap10 r -> op "WRAP10 %a" pp_reg r
  | Wrap10_ovi r -> op "WRAP10 %a, 1" pp_reg r
  | Ea -> op "EA"
  | Dispatch -> op "DISPATCH"
  | Mul10 (rd, rs) -> op "MUL10 %a, %a, 1" pp_reg rd pp_reg rs
  | Divchk (r, a) -> op "DIVCHK %a, %d" pp_reg r a
  | Absmod (rd, rs) -> op "ABSMOD %a, %a" pp_reg rd pp_reg rs
  | Cstore (r, At a) -> op "CSTORE %a, %d" pp_reg r a
  | Cstore (r, In rr) -> op "CSTORE %a, resreg=%a" pp_reg r pp_reg rr
