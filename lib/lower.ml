open Isa
open Riscv

let reg : Reg.t -> Riscv.reg = function
  | R0 -> S2
  | R1 -> S3
  | R2 -> S4
  | R3 -> S5
  | R4 -> S6
  | R5 -> S7
  | R6 -> S8
  | R7 -> S9
  | R8 -> S10
  | R9 -> S11

let sext n = if n >= 5000 then n - 10_000 else n
let set_cc r = Mv (S1, r)
let clear_ovi = Sd (Zero, -32, S0)
let step x k = [ Addi (x, x, k); Wrap10 x ]

type ix = {
  before : Riscv.instr list;
  rx : Riscv.reg;
  after : Riscv.instr list;
}

let ix : Index.t -> ix option = function
  | Unindexed -> None
  | Indexed r -> Some { before = []; rx = reg r; after = [] }
  | Pre_inc r -> Some { before = step (reg r) 1; rx = reg r; after = [] }
  | Post_inc r -> Some { before = []; rx = reg r; after = step (reg r) 1 }
  | Pre_dec r -> Some { before = step (reg r) (-1); rx = reg r; after = [] }
  | Post_dec r -> Some { before = []; rx = reg r; after = step (reg r) (-1) }

let idx dst offset { before; rx; after } =
  let add =
    if fits_imm offset then [ Addi (dst, rx, offset) ]
    else [ Li (dst, offset); Add (dst, dst, rx) ]
  in
  before @ add @ after

type value = Imm of int | In_reg of Riscv.reg | Loaded of Riscv.instr list

let value (mode : Mode.t) { n; index } =
  match (mode, ix index) with
  | Value, None -> Imm (sext n)
  | Address, None -> Imm n
  | (Value | Address), Some { before = []; rx; after = [] } when n = 0 ->
      In_reg rx
  | Value, Some i -> Loaded (idx T2 (sext n) i @ [ Wrap10 T2 ])
  | Address, Some i -> Loaded (idx T2 n i @ [ Wrap10 T2 ])
  | Direct, None -> Loaded [ Ld_word (T2, n) ]
  | Direct, Some i -> Loaded (idx T0 (sext n) i @ [ Ea; Ld (T2, 0, T0) ])
  | Indirect, None -> Loaded [ Ld_word (T0, n); Ea; Ld (T2, 0, T0) ]
  | Indirect, Some i ->
      Loaded (idx T0 (sext n) i @ [ Ea; Ld (T0, 0, T0); Ea; Ld (T2, 0, T0) ])

let in_reg = function
  | Imm n -> ([ Li (T2, n) ], T2)
  | In_reg r -> ([], r)
  | Loaded code -> (code, T2)

let arith ~sign make dst r v =
  match v with
  | Imm n when fits_imm (sign * n) -> [ Addi (dst, r, sign * n) ]
  | Imm _ | In_reg _ | Loaded _ ->
      let code, v = in_reg v in
      code @ [ make dst r v ]

let add dst a b = Add (dst, a, b)
let sub dst a b = Sub (dst, a, b)

type target = Known of int | Computed of Riscv.instr list

let target (t : Target.t) { n; index } =
  match (t, ix index) with
  | Direct, None -> Known n
  | Direct, Some i -> Computed (idx T0 (sext n) i)
  | Indirect, None -> Computed [ Ld_word (T0, n) ]
  | Indirect, Some i -> Computed (idx T0 (sext n) i @ [ Ea; Ld (T0, 0, T0) ])

type test = {
  setup : Riscv.instr list;
  cmp : cmp;
  lhs : Riscv.reg;
  rhs : Riscv.reg;
}

let test ?(setup = []) cmp lhs rhs = { setup; cmp; lhs; rhs }
let ovi = [ Ld (T1, -32, S0) ]

let condition : Cond.t -> test = function
  | NUL -> test Eq S1 Zero
  | NNUL -> test Ne S1 Zero
  | POS -> test Lt Zero S1 (* todo: is this correct? why not >= zero? *)
  | NPOS -> test Ge Zero S1
  | NEG -> test Lt S1 Zero
  | NNEG -> test Ge S1 Zero
  | OVL -> test ~setup:ovi Ne T1 Zero
  | GOVL -> test ~setup:ovi Eq T1 Zero
  | SO -> test Ltu S11 S9
  | GSO -> test Geu S11 S9

let branch { setup; cmp; lhs; rhs } l = setup @ [ B (cmp, lhs, rhs, l) ]

let jump ~is_code = function
  | Known a when is_code a -> [ J (Word a) ]
  | Known a -> [ Li (A0, a); Enter_interp ]
  | Computed code -> code @ [ Dispatch ]

let instr ~is_code ~addr = function
  | HIA (r, mode, o) ->
      let r = reg r in
      let load =
        match value mode o with
        | Imm n -> [ Li (r, n) ]
        | In_reg s -> [ Mv (r, s) ]
        | Loaded code -> code @ [ Mv (r, T2) ]
      in
      load @ [ set_cc r ]
  | OPT (r, mode, o) ->
      let r = reg r in
      arith ~sign:1 add r r (value mode o) @ [ Wrap10_ovi r; set_cc r ]
  | AFT (r, mode, o) ->
      let r = reg r in
      arith ~sign:(-1) sub r r (value mode o) @ [ Wrap10_ovi r; set_cc r ]
  | VER (r, mode, o) ->
      let r = reg r in
      let code, v = in_reg (value mode o) in
      code @ [ Mul10 (r, v); set_cc r ]
  | DEL (r, mode, o) ->
      let r = reg r in
      let code, v = in_reg (value mode o) in
      code @ [ Divchk (v, addr); Div (r, r, v); Wrap10 r; set_cc r; clear_ovi ]
  | MOD (r, mode, o) ->
      let r = reg r in
      let code, v = in_reg (value mode o) in
      code @ [ Divchk (v, addr); Absmod (r, v); set_cc r; clear_ovi ]
  | VGL (r, mode, o) ->
      arith ~sign:(-1) sub T1 (reg r) (value mode o) @ [ Wrap10 T1; set_cc T1 ]
  | BIG (r, t, o) -> (
      let r = reg r in
      match target t o with
      | Known a when is_code a ->
          [ Lla_word (T0, a); set_cc r; Cstore (r, At (addr + 1)) ]
      | Known a -> [ Sd_word (r, a, T0); set_cc r ]
      | Computed code -> code @ [ Ea; set_cc r; Cstore (r, At (addr + 1)) ])
  | SPR (t, o) -> jump ~is_code (target t o)
  | VSP (c, t, o) -> (
      let test = condition c in
      let unless body =
        branch { test with cmp = negate test.cmp } (Skip addr)
        @ body @ [ Label (Skip addr) ]
      in
      match target t o with
      | Known a when is_code a -> branch test (Word a)
      | Known a -> unless (jump ~is_code (Known a))
      | Computed code -> code @ unless [ Dispatch ])
  | SBR (t, o) -> (
      let push = step S11 (-1) @ [ Mv (T0, S11); Ea; Li (T1, addr + 1) ] in
      match target t o with
      | Known a -> push @ [ Cstore (T1, At a) ] @ jump ~is_code (Known a)
      | Computed code ->
          code
          @ [ Mv (T2, T0) ]
          @ push
          @ [ Cstore (T1, In T2); Mv (T0, T2); Dispatch ])
  | KTG -> [ Mv (T0, S11); Ea; Ld (T0, 0, T0) ] @ step S11 1 @ [ Dispatch ]
  | LEZ -> [ Call Lez; Mv (S2, A0); set_cc S2 ]
  | DRU -> [ Mv (A0, S2); Call Dru; set_cc S2 ]
  | DRS -> [ Mv (A0, S2); Call Drs ]
  | NWL -> [ Call Nwl ]
  | NTS -> []
  | STP -> [ Call Stp ]

let falls_through = function
  | SPR _ | SBR _ | KTG | STP -> false
  | HIA _ | OPT _ | AFT _ | VER _ | DEL _ | MOD _ | VGL _ | BIG _ | VSP _ | LEZ
  | DRU | DRS | NWL | NTS ->
      true

let decode image addr = Encoding.decode (Assembler.word image addr)
let is_code image addr = Option.is_some (decode image addr)

let program image =
  let is_code = is_code image in
  let size = Assembler.memory_size in
  let fall_into addr =
    if addr < size && is_code addr then []
    else jump ~is_code (Known (addr mod size))
  in
  let word addr =
    match decode image addr with
    | None -> []
    | Some i ->
        Label (Word addr)
        :: Comment (Format.asprintf "%a" (Isa.pp Format.pp_print_int) i)
        :: instr ~is_code ~addr i
        @ if falls_through i then fall_into (addr + 1) else []
  in
  fall_into 0 @ (List.init size Fun.id |> List.concat_map word)
