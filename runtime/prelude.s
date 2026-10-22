# s0 = &drama_mem[0], s1 = CC witness, s2..s11 = R0..R9
# Pool: -32(s0) = OVI, -24(s0) = 10000, -16(s0) = 10^10, -8(s0) = 5*10^9

.macro NOT_IN r, bad:vararg
.irp b, \bad
.ifc \r, \b
    .error "macro operand \r conflicts with scratch register \b"
.endif
.endr
.endm

# WRAP10 r [,ov]: r := wrap10(r) valid for r in (-1.5^10, 1.5^10)
# ov = 1 sets OVI := (r out of range)
# clobbers t3, t4, t5
.macro WRAP10 r, ov=0
    NOT_IN \r, t3, t4, t5
    ld    t5, -8(s0)
    add   t4, \r, t5
    ld    t3, -16(s0)
.if \ov
    sltu  t5, t4, t3
    xori  t5, t5, 1
    sd    t5, -32(s0)
.endif
    bltu  t4, t3, .Lwrap_done\@       # fast path
    bgez  \r, .Lwrap_pos\@
    add   \r, \r, t3
    j     .Lwrap_done\@
.Lwrap_pos\@:  
    sub   \r, \r, t3
.Lwrap_done\@:
.endm

# EA: t0 := &drama_mem[t0 mod 10000]
# clobbers t6
.macro EA
    ld    t6, -24(s0)
    bltu  t0, t6, .Lea_ok\@      # fast path: 0 <= t0 < 10000
    rem   t0, t0, t6
    bgez  t0, .Lea_ok\@
    add   t0, t0, t6
.Lea_ok\@:  
    slli  t0, t0, 3
    add   t0, t0, s0
.endm

# DISPATCH: BT := t0 mod 10000; jump to its code
# clobbers t1, t6, a0
.macro DISPATCH
    ld    t6, -24(s0)
    bltu  t0, t6, .Ldisp_ok\@
    rem   t0, t0, t6
    bgez  t0, .Ldisp_ok\@
    add   t0, t0, t6
.Ldisp_ok\@:  
    mv    a0, t0
    lla   t1, drama_code_table
    slli  t0, t0, 2
    add   t0, t0, t1
    lw    t0, 0(t0)
    add   t0, t0, t1
    jr    t0
.endm

# MUL10 rd, rs [,ov]: rd := wrap10(rd * rs)
# clobbers t1, t3, t4, t5 and on wide path every caller-saved register
.macro MUL10 rd, rs, ov=0
    NOT_IN \rd, t1, t3, t4, t5
    NOT_IN \rs, t1, t3, t4, a0
    mul   t1, \rd, \rs
    mulh  t3, \rd, \rs
    srai  t4, t1, 63
    bne   t3, t4, .Lmul_wide\@     # product needs > 64 bits
    ld    t5, -8(s0)
    add   t4, t1, t5
    ld    t3, -16(s0)
    bgeu  t4, t3, .Lmul_big\@      # fits in 64 bits but not in 10 digits
    mv    \rd, t1
.if \ov
    sd    zero, -32(s0)
.endif
    j     .Lmul_done\@
.Lmul_big\@:  
    rem   \rd, t1, t3
    WRAP10 \rd
    j     .Lmul_ovl\@
.Lmul_wide\@:  
    mv    a0, \rd
    mv    a1, \rs
    call  __drama_mul_wide
    mv    \rd, a0
.Lmul_ovl\@:
.if \ov
    li    t5, 1
    sd    t5, -32(s0)
.endif
.Lmul_done\@:
.endm

# DIVCHK r, addr: runtime error if r == 0
.macro DIVCHK r, addr
    bnez \r, .Ldiv_ok\@
    li   a0, \addr
    call __drama_err_div0
.Ldiv_ok\@:
.endm

# ABSMOD rd, rs: rd := rd mod |rs|
# clobbers t1, t3, t4
.macro ABSMOD rd, rs
    NOT_IN \rs, t1, t3
    rem  t1, \rd, \rs
    bgez t1, .Labsmod_done\@
    srai t3, \rs, 63
    xor  t4, \rs, t3
    sub  t4, t4, t3      # t4 = |rs|
    add  t1, t1, t4
.Labsmod_done\@:  
    mv   \rd, t1
.endm

# CSTORE r, resume [, resreg]: mem[t0] := r (t0 = pointer from EA)
# if the word was code at compile time and the value changes, continue in the
# interpreter at DRAMA address `resume` (or the value in register `resreg`)
# clobbers t3, t4, a0
.macro CSTORE r, resume, resreg
    NOT_IN \r, t3, t4
.ifb \resreg
.ifb \resume
    .error "CSTORE needs a resume address or resreg"
.endif
.else
    NOT_IN \resreg, t3, t4
.endif
    lla  t3, drama_codemap
    sub  t4, t0, s0
    srli t4, t4, 3
    add  t3, t3, t4
    lbu  t3, 0(t3)
    bnez t3, .Lcst_code\@
    sd   \r, 0(t0)     # fast path: not a code word
    j    .Lcst_done\@
.Lcst_code\@:
    ld   t3, 0(t0)
    sd   \r, 0(t0)
    beq  t3, \r, .Lcst_done\@    # value didn't change
.ifnb \resreg
    mv   a0, \resreg
.else
    li   a0, \resume
.endif
    tail __drama_enter_interp
.Lcst_done\@:
.endm
