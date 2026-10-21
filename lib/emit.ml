open Riscv

let memory_size = Assembler.memory_size
let line ppf fmt = Format.fprintf ppf (fmt ^^ "@\n")
let instrs ppf = List.iter (line ppf "%a" pp)
let signed w = if w >= 5_000_000_000 then w - 10_000_000_000 else w

let prologue =
  [ Lla_word (S0, 0); Li (S1, 0) ]
  @ List.map (fun r -> Li (r, 0)) [ S2; S3; S4; S5; S6; S7; S8; S9; S10 ]
  @ [ Li (S11, 9000) ]

let text ppf image =
  line ppf {| .include "prelude.s" |};
  line ppf " .text";
  line ppf " .global main";
  line ppf "main:";
  instrs ppf prologue;
  instrs ppf (Lower.program image);
  instrs ppf [ Label BadJump; Enter_interp ]

let code_table ppf image addrs =
  let entry a =
    let target = if Lower.is_code image a then Word a else BadJump in
    line ppf " .word %a - drama_code_table" pp_label target
  in
  line ppf " .balign 4";
  line ppf "drama_code_table:";
  List.iter entry addrs;
  line ppf " .rept %d" (memory_size - List.length addrs);
  line ppf " .word %a - drama_code_table" pp_label BadJump;
  line ppf " .endr"

let codemap ppf image addrs =
  let flag a = if Lower.is_code image a then 1 else 0 in
  line ppf " .section .rodata";
  line ppf "drama_codemap:";
  List.iter (fun a -> line ppf " .byte %d" (flag a)) addrs;
  line ppf " .zero %d" (memory_size - List.length addrs)

let data ppf image addrs =
  line ppf " .data";
  line ppf " .balign 8";
  line ppf " .globl drama_pool";
  line ppf "drama_pool:";
  line ppf " .dword 0, 10000, 10000000000, 5000000000";
  line ppf " .globl drama_mem";
  line ppf "drama_mem:";
  List.iter
    (fun a -> line ppf " .dword %d" (signed (Assembler.word image a)))
    addrs;
  line ppf " .zero %d" (8 * (memory_size - List.length addrs))

let program ppf image =
  let addrs = List.init (Assembler.size image) Fun.id in
  text ppf image;
  code_table ppf image addrs;
  codemap ppf image addrs;
  data ppf image addrs
