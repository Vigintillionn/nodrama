#include <stdint.h>
#include <stddef.h>

#define MEM_SIZE 10000
#define MOD10 10000000000LL /* 10^10 */
#define HALF (MOD10 / 2)    /* values live in [-HALF, HALF) */

#define CHAR_BASE 1000 /* 3 decimal digits per char */
#define CHARS_PER_CELL 3
#define CELL_BASE 1000000000LL /* CHAR_BASE ^ CHARS_PER_CELL */

#define STDIN 0
#define STDOUT 1
#define STDERR 2

#define INT_CHARS 20 /* sign + 19 digits of an int64 */

long __sys_read(int fd, void *buf, size_t len);
long __sys_write(int fd, const void *buf, size_t len);
_Noreturn void __sys_exit(int status);

extern int64_t drama_mem[MEM_SIZE];

static void write_all(int fd, const char *p, size_t len)
{
    while (len > 0)
    {
        long n = __sys_write(fd, p, len);
        if (n <= 0)
            __sys_exit(1);
        p += n;
        len -= (size_t)n;
    }
}

static size_t str_len(const char *s)
{
    size_t len = 0;
    while (*s++)
        len++;
    return len;
}

static void write_str(int fd, const char *s) { write_all(fd, s, str_len(s)); }

static char out_buf[4096];
static size_t out_len;

static void flush(void)
{
    write_all(STDOUT, out_buf, out_len);
    out_len = 0;
}

static void put_char(char c)
{
    if (out_len == sizeof out_buf)
        flush();
    out_buf[out_len++] = c;
}

static char *format_int(char *end, int64_t v)
{
    uint64_t m = v < 0 ? -(uint64_t)v : (uint64_t)v;
    char *p = end;
    do
    {
        *--p = (char)('0' + (m % 10));
        m /= 10;
    } while (m != 0);

    if (v < 0)
        *--p = '-';
    return p;
}

static _Noreturn void fatal(const char *msg, long long v)
{
    char num[INT_CHARS];
    char *end = num + sizeof num;
    char *start = format_int(end, v);

    flush();
    write_str(STDERR, "DRAMA runtime fout: ");
    write_str(STDERR, msg);
    write_str(STDERR, " (");
    write_all(STDERR, start, (size_t)(end - start));
    write_str(STDERR, ")\n");
    __sys_exit(1);
}

static char in_buf[4096];
static size_t in_pos, in_len;

/* The next byte of stdin or -1 at eoi */
static int next_byte(void)
{
    if (in_pos == in_len)
    {
        long n = __sys_read(STDIN, in_buf, sizeof in_buf);
        if (n <= 0)
            return -1;
        in_pos = 0;
        in_len = (size_t)n;
    }
    return (unsigned char)in_buf[in_pos++];
}

static int is_blank(int c) { return c == ' ' || c == '\t' || c == '\r'; }
static int is_digit(int c) { return c >= '0' && c <= '9'; }

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
    flush();

    int c = next_byte();
    if (c < 0)
        fatal("Geen invoer beschikbaar", 0);
    while (is_blank(c))
        c = next_byte();

    int negative = c == '-';
    if (c == '-' || c == '+')
        c = next_byte();
    if (!is_digit(c))
        fatal("Ongeldige invoer", 0);

    int64_t v = 0;
    int too_big = 0;
    for (; is_digit(c); c = next_byte())
    {
        if (v > (INT64_MAX - 9) / 10)
            too_big = 1;
        else
            v = v * 10 + (c - '0');
    }
    while (is_blank(c))
        c = next_byte();

    if ((c != '\n' && c >= 0) || too_big)
        fatal("Ongeldige invoer", 0);

    if (negative)
        v = -v;
    if (v < -HALF || v >= HALF)
        fatal("Ongeldige invoer (buiten bereik)", v);
    return v;
}

void __drama_dru(int64_t v)
{
    char num[INT_CHARS];
    char *end = num + sizeof num;
    for (char *p = format_int(end, v); p < end; p++)
        put_char(*p);
    put_char('\t'); // IDK drama spec says to end with a tab which is weird but whatever
}
void __drama_nwl(void) { put_char('\n'); }

static void put_cp(unsigned c)
{
    if (c < 0x80)
        put_char(c);
    else
    {
        put_char(0xC0 | (c >> 6));
        put_char(0x80 | (c & 0x3F));
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

_Noreturn void __drama_stp(void)
{
    flush();
    __sys_exit(0);
}
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