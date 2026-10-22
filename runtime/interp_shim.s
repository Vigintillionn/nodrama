    .text
    .globl __drama_enter_interp
__drama_enter_interp:
    lla t0, drama_state
    sd   s1,  0(t0)                     # CC witness
    sd   s2,  8(t0)                     # R0
    sd   s3, 16(t0)
    sd   s4, 24(t0)
    sd   s5, 32(t0)
    sd   s6, 40(t0)
    sd   s7, 48(t0)
    sd   s8, 56(t0)
    sd   s9, 64(t0)
    sd   s10, 72(t0)
    sd   s11, 80(t0)                    # R9
    tail __drama_interp

    .bss
    .balign 8
    .globl drama_state
drama_state: .zero 88
