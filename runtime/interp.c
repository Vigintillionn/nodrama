#include "runtime.h"

enum opcode
{
    OP_HIA = 11,
    OP_BIG = 12,
    OP_OPT = 21,
    OP_AFT = 22,
    OP_VER = 23,
    OP_DEL = 24,
    OP_MOD = 25,
    OP_VGL = 31,
    OP_SPR = 32,
    OP_VSP = 33,
    OP_SBR = 41,
    OP_KTG = 42,
    OP_LEZ = 71,
    OP_DRU = 72,
    OP_NWL = 73,
    OP_NTS = 74,
    OP_DRS = 79,
    OP_STP = 99,
};

enum mode
{
    M_VALUE = 1,
    M_ADDRESS = 2,
    M_DIRECT = 3,
    M_INDIRECT = 4,
};

enum target
{
    T_DIRECT = 2,
    T_INDIRECT = 3,
};

enum index_form
{
    IX_NONE = 1,
    IX_REG = 2,
    IX_PRE_INC = 3,
    IX_POST_INC = 4,
    IX_PRE_DEC = 5,
    IX_POST_DEC = 6,
};

enum cond
{
    C_SO = 0,
    C_NUL = 1,
    C_NNEG = 2,
    C_NPOS = 3,
    C_OVL = 4,
    C_GOVL = 5,
    C_POS = 6,
    C_NEG = 7,
    C_NNUL = 8,
    C_GSO = 9,
};

struct fields
{
    int op, m, c, a, x, n;
};

#define OVI drama_pool[0]

static int64_t mem_index(int64_t a)
{
    int64_t i = a % MEM_SIZE;
    return i < 0 ? i + MEM_SIZE : i;
}

static struct fields unpack(int64_t v)
{
    uint64_t w = (uint64_t)(v < 0 ? v + MOD10 : v);
    return (struct fields){
        .op = (int)(w / 100000000),
        .m = (int)(w / 10000000 % 10),
        .c = (int)(w / 1000000 % 10),
        .a = (int)(w / 100000 % 10),
        .x = (int)(w / 10000 % 10),
        .n = (int)(w % 10000),
    };
}

static int is_valid(struct fields f)
{
    int indexed = f.c >= IX_NONE && f.c <= IX_POST_DEC;
    int target = indexed && (f.m == T_DIRECT || f.m == T_INDIRECT);
    switch (f.op)
    {
    case OP_HIA:
    case OP_OPT:
    case OP_AFT:
    case OP_VER:
    case OP_DEL:
    case OP_MOD:
    case OP_VGL:
        return indexed && f.m >= M_VALUE && f.m <= M_INDIRECT;
    case OP_BIG:
    case OP_VSP:
        return target;
    case OP_SPR:
    case OP_SBR:
        return target && f.a == 9;
    case OP_KTG:
    case OP_LEZ:
    case OP_DRU:
    case OP_NWL:
    case OP_NTS:
    case OP_DRS:
    case OP_STP:
        return f.m == 9 && f.c == 9 && f.a == 9 && f.x == 9 && f.n == 9999;
    default:
        return 0;
    }
}

/* Applies the index side effect, so it must run before the instruction reads Rr */
static int64_t index_value(struct fields f, int64_t *r)
{
    int64_t *rx = &r[f.x];
    int64_t old = *rx;
    switch (f.c)
    {
    case IX_REG:
        return old;
    case IX_PRE_INC:
        return *rx = wrap10(old + 1);
    case IX_POST_INC:
        *rx = wrap10(old + 1);
        return old;
    case IX_PRE_DEC:
        return *rx = wrap10(old - 1);
    case IX_POST_DEC:
        *rx = wrap10(old - 1);
        return old;
    default:
        return 0;
    }
}

static int64_t operand_value(struct fields f, int64_t *r)
{
    int64_t ix = index_value(f, r);
    switch (f.m)
    {
    case M_VALUE:
        return wrap10((f.n >= 5000 ? f.n - 10000 : f.n) + ix);
    case M_ADDRESS:
        return wrap10(f.n + ix);
    case M_DIRECT:
        return drama_mem[mem_index(f.n + ix)];
    default:
        return drama_mem[mem_index(drama_mem[mem_index(f.n + ix)])];
    }
}

static int64_t operand_target(struct fields f, int64_t *r)
{
    int64_t a = mem_index(f.n + index_value(f, r));
    return f.m == T_DIRECT ? a : mem_index(drama_mem[a]);
}

static int holds(int cond, int64_t cc, const int64_t *r)
{
    int so = (uint64_t)r[9] < (uint64_t)r[7];
    switch (cond)
    {
    case C_SO:
        return so;
    case C_NUL:
        return cc == 0;
    case C_NNEG:
        return cc >= 0;
    case C_NPOS:
        return cc <= 0;
    case C_OVL:
        return OVI != 0;
    case C_GOVL:
        return OVI == 0;
    case C_POS:
        return cc > 0;
    case C_NEG:
        return cc < 0;
    case C_NNUL:
        return cc != 0;
    default:
        return !so;
    }
}

static int64_t checked(__int128 x)
{
    OVI = x < -HALF || x >= HALF;
    return wrap10((int64_t)(x % MOD10));
}

/* CC is kept as a witness value whose sign is CC (spec §7.2) */
_Noreturn void __drama_interp(int64_t bt)
{
    int64_t *r = drama_state.r;
    int64_t cc = drama_state.cc;
    bt = mem_index(bt);

    for (;;)
    {
        int64_t addr = bt;
        struct fields f = unpack(drama_mem[addr]);
        if (!is_valid(f))
            fatal("Niet decodeerbare instructie op adres", addr);
        bt = (bt + 1) % MEM_SIZE;

        int64_t *acc = &r[f.a];
        int64_t v, t;
        switch (f.op)
        {
        case OP_HIA:
            v = operand_value(f, r);
            cc = *acc = v;
            break;
        case OP_OPT:
            v = operand_value(f, r);
            cc = *acc = checked((__int128)*acc + v);
            break;
        case OP_AFT:
            v = operand_value(f, r);
            cc = *acc = checked((__int128)*acc - v);
            break;
        case OP_VER:
            v = operand_value(f, r);
            cc = *acc = checked((__int128)*acc * v);
            break;
        case OP_DEL:
            v = operand_value(f, r);
            if (v == 0)
                __drama_err_div0(addr);
            cc = *acc = wrap10(*acc / v);
            OVI = 0;
            break;
        case OP_MOD:
            v = operand_value(f, r);
            if (v == 0)
                __drama_err_div0(addr);
            v = v < 0 ? -v : v;
            cc = *acc = (*acc % v + v) % v;
            OVI = 0;
            break;
        case OP_VGL:
            v = operand_value(f, r);
            cc = wrap10(*acc - v);
            break;
        case OP_BIG:
            t = operand_target(f, r);
            cc = drama_mem[t] = *acc;
            break;
        case OP_SPR:
            bt = operand_target(f, r);
            break;
        case OP_VSP:
            t = operand_target(f, r);
            if (holds(f.a, cc, r))
                bt = t;
            break;
        case OP_SBR:
            t = operand_target(f, r);
            r[9] = wrap10(r[9] - 1);
            drama_mem[mem_index(r[9])] = bt;
            bt = t;
            break;
        case OP_KTG:
            bt = mem_index(drama_mem[mem_index(r[9])]);
            r[9] = wrap10(r[9] + 1);
            break;
        case OP_LEZ:
            cc = r[0] = __drama_lez();
            break;
        case OP_DRU:
            __drama_dru(r[0]);
            cc = r[0];
            break;
        case OP_NWL:
            __drama_nwl();
            break;
        case OP_DRS:
            __drama_drs(r[0]);
            break;
        case OP_NTS:
            break;
        case OP_STP:
            __drama_stp();
        }
    }
}