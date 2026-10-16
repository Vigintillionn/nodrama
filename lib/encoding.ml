open Isa

type fields = { op : int; m : int; c : int; a : int; x : int; n : int }

let op_off = 100_000_000
let mode_off = 10_000_000
let index_form_off = 1_000_000
let acc_off = 100_000
let index_reg_off = 10_000

let pack { op; m; c; a; x; n } =
  (op * op_off) + (m * mode_off) + (c * index_form_off) + (a * acc_off)
  + (x * index_reg_off) + n

let unpack w =
  {
    op = w / op_off;
    m = w / mode_off mod 10;
    c = w / index_form_off mod 10;
    a = w / acc_off mod 10;
    x = w / index_reg_off mod 10;
    n = w mod 10_000;
  }

let mode_digit : Mode.t -> int = function
  | Value -> 1
  | Address -> 2
  | Direct -> 3
  | Indirect -> 4

let mode_of_digit : int -> Mode.t option = function
  | 1 -> Some Value
  | 2 -> Some Address
  | 3 -> Some Direct
  | 4 -> Some Indirect
  | _ -> None

let target_digit : Target.t -> int = function Direct -> 2 | Indirect -> 3

let target_of_digit : int -> Target.t option = function
  | 2 -> Some Direct
  | 3 -> Some Indirect
  | _ -> None

let cond_digit : Cond.t -> int = function
  | SO -> 0
  | NUL -> 1
  | NNEG -> 2
  | NPOS -> 3
  | OVL -> 4
  | GOVL -> 5
  | POS -> 6
  | NEG -> 7
  | NNUL -> 8
  | GSO -> 9

let cond_of_digit : int -> Cond.t option = function
  | 0 -> Some SO
  | 1 -> Some NUL
  | 2 -> Some NNEG
  | 3 -> Some NPOS
  | 4 -> Some OVL
  | 5 -> Some GOVL
  | 6 -> Some POS
  | 7 -> Some NEG
  | 8 -> Some NNUL
  | 9 -> Some GSO
  | _ -> None

let index_digits : Index.t -> int * int = function
  | Unindexed -> (1, 0)
  | Indexed r -> (2, Reg.to_int r)
  | Pre_inc r -> (3, Reg.to_int r)
  | Post_inc r -> (4, Reg.to_int r)
  | Pre_dec r -> (5, Reg.to_int r)
  | Post_dec r -> (6, Reg.to_int r)

let index_of_digits c x : Index.t option =
  let indexed (make : Reg.t -> Index.t) = Reg.of_int x |> Option.map make in
  match c with
  | 1 -> Some Unindexed
  | 2 -> indexed (fun r -> Indexed r)
  | 3 -> indexed (fun r -> Pre_inc r)
  | 4 -> indexed (fun r -> Post_inc r)
  | 5 -> indexed (fun r -> Pre_dec r)
  | 6 -> indexed (fun r -> Post_dec r)
  | _ -> None

let with_operand ~op ~m ~a { n; index } =
  if n < 0 || n > 9999 then invalid_arg "Encoding.encode: operand out of range";
  let c, x = index_digits index in
  pack { op; m; c; a; x; n }

let class1 op r mode o =
  with_operand ~op ~m:(mode_digit mode) ~a:(Reg.to_int r) o

let addr op ~a target o = with_operand ~op ~m:(target_digit target) ~a o
let class0 op = (op * op_off) + 99_999_999

let encode = function
  | HIA (r, mode, o) -> class1 11 r mode o
  | OPT (r, mode, o) -> class1 21 r mode o
  | AFT (r, mode, o) -> class1 22 r mode o
  | VER (r, mode, o) -> class1 23 r mode o
  | DEL (r, mode, o) -> class1 24 r mode o
  | MOD (r, mode, o) -> class1 25 r mode o
  | VGL (r, mode, o) -> class1 31 r mode o
  | BIG (r, target, o) -> addr 12 ~a:(Reg.to_int r) target o
  | SPR (target, o) -> addr 32 ~a:9 target o
  | VSP (cond, target, o) -> addr 33 ~a:(cond_digit cond) target o
  | SBR (target, o) -> addr 41 ~a:9 target o
  | KTG -> class0 42
  | LEZ -> class0 71
  | DRU -> class0 72
  | NWL -> class0 73
  | NTS -> class0 74
  | DRS -> class0 79
  | STP -> class0 99

let ( let* ) = Option.bind

let decode_valid w =
  let f = unpack w in
  let operand () =
    let* index = index_of_digits f.c f.x in
    Some { n = f.n; index }
  in
  let class1 make =
    let* r = Reg.of_int f.a in
    let* mode = mode_of_digit f.m in
    let* o = operand () in
    Some (make r mode o)
  in
  let addr make =
    let* target = target_of_digit f.m in
    let* o = operand () in
    Some (make target o)
  in
  let no_acc make = if f.a = 9 then addr make else None in
  let class0 instr = if w mod op_off = 99_999_999 then Some instr else None in
  match f.op with
  | 11 -> class1 (fun r m o -> HIA (r, m, o))
  | 21 -> class1 (fun r m o -> OPT (r, m, o))
  | 22 -> class1 (fun r m o -> AFT (r, m, o))
  | 23 -> class1 (fun r m o -> VER (r, m, o))
  | 24 -> class1 (fun r m o -> DEL (r, m, o))
  | 25 -> class1 (fun r m o -> MOD (r, m, o))
  | 31 -> class1 (fun r m o -> VGL (r, m, o))
  | 12 ->
      let* r = Reg.of_int f.a in
      addr (fun t o -> BIG (r, t, o))
  | 32 -> no_acc (fun t o -> SPR (t, o))
  | 33 ->
      let* cond = cond_of_digit f.a in
      addr (fun t o -> VSP (cond, t, o))
  | 41 -> no_acc (fun t o -> SBR (t, o))
  | 42 -> class0 KTG
  | 71 -> class0 LEZ
  | 72 -> class0 DRU
  | 73 -> class0 NWL
  | 74 -> class0 NTS
  | 79 -> class0 DRS
  | 99 -> class0 STP
  | _ -> None

let decode w = if w < 0 || w >= 10_000_000_000 then None else decode_valid w
