#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#define MEM_SIZE 10000
#define MOD10 10000000000LL /* 10^10 */
#define HALF (MOD10 / 2)    /* values live in [-HALF, HALF) */

#define CHAR_BASE 1000 /* 3 decimal digits per char */
#define CHARS_PER_CELL 3
#define CELL_BASE 1000000000LL /* CHAR_BASE ^ CHARS_PER_CELL */

extern int64_t drama_mem[MEM_SIZE];

static _Noreturn void fatal(const char *msg, long long v)
{
    fflush(stdout);
    fprintf(stderr, "DRAMA runtime error: %s (%lld)\n", msg, v);
    exit(EXIT_FAILURE);
}

static int64_t wrap10(int64_t x)
{
    if (x >= HALF)
        return x - MOD10;
    if (x < -HALF)
        return x + MOD10;
    return x;
}

int64_t __drama_mul_wide(int64_t a, int64_t b)
{
    __int128 p = (__int128)a * b;
    return wrap10((int64_t)(p % MOD10));
}

int64_t __drama_lez(void)
{
    char buf[256];
    char *end;

    fflush(stdout);

    if (!fgets(buf, sizeof buf, stdin))
        fatal("Geen invoer beschikbaar", 0);
    if (!strchr(buf, '\n') && !feof(stdin))
        fatal("Ongeldige invoer (te lang)", 0);

    errno = 0;
    long long v = strtoll(buf, &end, 10);
    if (end == buf || errno == ERANGE)
        fatal("Ongeldige invoer", 0);
    while (*end == ' ' || *end == '\t' || *end == '\r' || *end == '\n')
        end++;

    if (*end != '\0')
        fatal("Ongeldige invoer", 0);
    if (v < -HALF || v >= HALF)
        fatal("Ongeldige invoer (buiten bereik)", v);

    return (int64_t)v;
}

void __drama_dru(int64_t v) { printf("%lld\t", (long long)v); }
void __drama_nwl(void) { putchar('\n'); }

static void put_cp(unsigned c)
{
    if (c < 0x80)
        putchar(c);
    else
    {
        putchar(0xC0 | (c >> 6));
        putchar(0x80 | (c & 0x3F));
    }
}

void __drama_drs(int64_t r0)
{
    int64_t a = ((r0 % MEM_SIZE) + MEM_SIZE) % MEM_SIZE;

    for (int n = 0; n < MEM_SIZE; n++)
    {
        int64_t v = drama_mem[a];
        a = (a + 1) % MEM_SIZE;

        uint64_t w = (uint64_t)(((v % CELL_BASE) + CELL_BASE) % CELL_BASE);
        for (int k = 0; k < CHARS_PER_CELL; k++)
        {
            unsigned c = (unsigned)(w % CHAR_BASE);
            if (c == 0)
                return;
            put_cp(c);
            w /= CHAR_BASE;
        }
    }
}

_Noreturn void __drama_stp(void) { exit(0); }
_Noreturn void __drama_err_div0(int64_t addr)
{
    fatal("Deling door nul op adres", addr);
}

struct drama_state
{
    int64_t cc;
    int64_t r[10];
};
extern struct drama_state drama_state;
extern int64_t drama_pool[4];

_Noreturn void __drama_interp(int64_t bt)
{
    fatal("Zelfmodificerende code of sprong naar data van niet ondersteund adres", bt);
}