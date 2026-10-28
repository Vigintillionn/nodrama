#ifndef DRAMA_RUNTIME_H
#define DRAMA_RUNTIME_H

#include <stdint.h>

#define MEM_SIZE 10000
#define MOD10 10000000000LL /* 10^10 */
#define HALF (MOD10 / 2)    /* values live in [-HALF, HALF) */

struct drama_state
{
    int64_t cc;
    int64_t r[10];
};

/* Defined by the generated assembly and interp_shim.s */
extern int64_t drama_mem[MEM_SIZE];
extern int64_t drama_pool[4];
extern struct drama_state drama_state;

_Noreturn void fatal(const char *msg, long long v);

static inline int64_t wrap10(int64_t x)
{
    if (x >= HALF)
        return x - MOD10;
    if (x < -HALF)
        return x + MOD10;
    return x;
}

/* Entry points */
int64_t __drama_mul_wide(int64_t a, int64_t b);
int64_t __drama_lez(void);
void __drama_dru(int64_t v);
void __drama_nwl(void);
void __drama_drs(int64_t r0);
_Noreturn void __drama_stp(void);
_Noreturn void __drama_err_div0(int64_t addr);
_Noreturn void __drama_interp(int64_t bt);

#endif
