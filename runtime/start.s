    .text
    .globl _start
_start:
    .option push
    .option norelax
    lla    gp, __global_pointer$
    .option pop
    tail main

    .globl __sys_read
__sys_read:
    li     a7, 63
    ecall
    ret

    .globl __sys_write
__sys_write:
    li     a7, 64
    ecall
    ret

    .globl __sys_exit
__sys_exit:
    li     a7, 94
    ecall
    