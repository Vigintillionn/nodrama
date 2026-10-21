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

type label =
  | Word of int  (** [.Lw<a>] the code of DRAMA address [a] *)
  | Skip of int  (** a local label inside the code of DRAMA address [a] *)
  | BadJump  (** [.Lbadjump] where [DISPATCH] lands on a word without code *)

type cmp = Eq | Ne | Lt | Ge | Ltu | Geu
type routine = Lez | Dru | Drs | Nwl | Stp

(** Where interpreter continues if a [Cstore] changes a code word *)
type resume = At of int | In of reg

(** One line of assembly. [Wrap10] to [Cstore] are prelude macros *)
type instr =
  | Label of label
  | Comment of string
  | Li of reg * int
  | Mv of reg * reg
  | Addi of reg * reg * int  (** immediate must satisfy [fits_imm] *)
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Div of reg * reg * reg
  | Ld of reg * int * reg  (** [Ld (rd, offset, base)] *)
  | Sd of reg * int * reg  (** [Sd (rs, offset, base)] *)
  | Ld_word of reg * int  (** [Ld_word (rd, a)] loads DRAMA word [a] *)
  | Sd_word of reg * int * reg
      (** [Sd_word (rs, a, scratch)] stores to DRAMA word [a] *)
  | Lla_word of reg * int  (** address of DRAMA word [a] *)
  | B of cmp * reg * reg * label
  | J of label
  | Call of routine
  | Enter_interp  (** tail call into the interpreter, [a0] = DRAMA address *)
  | Wrap10 of reg
  | Wrap10_ovi of reg  (** [Wrap10] that also sets OVI *)
  | Ea
  | Dispatch
  | Mul10 of reg * reg  (** sets OVI *)
  | Divchk of reg * int  (** [Divchk (r, a)]: error at DRAMA address [a] *)
  | Absmod of reg * reg
  | Cstore of reg * resume

val negate : cmp -> cmp
val fits_imm : int -> bool
val pp_label : Format.formatter -> label -> unit
val pp : Format.formatter -> instr -> unit
