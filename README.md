# NoDRAMA

A compiler for DRAMA, the little decimal assembly language that was used in the SOCS
course at KU Leuven. It takes a `.dra` file and turns it into RISC-V assembly
(RV64IM, Linux), which you can then assemble, link and run like any other program.

Mostly a side project for learning OCaml.

It's still a work in progress. Normal programs work. Self-modifying code and jumping
into data don't yet, and the runtime stops with an error when that happens.

## Building

You need OCaml 5.1 or newer with opam. To link and run the programs it produces, you
also need a RISC-V Linux cross compiler and qemu:

```sh
pacman -S riscv64-linux-gnu-gcc qemu-user           # Arch
apt install gcc-riscv64-linux-gnu qemu-user         # Debian/Ubuntu
```

Then:

```sh
git clone https://github.com/Vigintillionn/nodrama.git
cd nodrama
opam install . --deps-only
dune build
```

Run `dune install` to put `nodrama` on your path. The runtime is built into the
binary, so it works from any directory. Otherwise use `dune exec nodrama --`.

## Usage

```sh
nodrama examples/sum.dra
qemu-riscv64 ./sum
```

This should print `145`. The executable is named after the input, in the current
directory. Use `-o` to pick another name.

```
-o FILE     write the output to FILE
-S          write RISC-V assembly (sum.s) instead of an executable; -o - for stdout
--cc CMD    cross compiler used for linking (default riscv64-linux-gnu-gcc, or set NODRAMA_CC)
```

There are also some flags to stop partway through, which help when debugging:

```
--tokens   print the tokens of each line
--ast      print the parsed program
--image    print the symbol table and the assembled memory image
```

`nodrama --help` shows everything.

## DRAMA in short

This is a cheat sheet/spec I built myself from the course notes and decompiling an emulator I found in someone's random github.

### The machine

- Memory is 10 000 words, addresses 0 to 9999. Code and data live in the same memory,
  and the program is loaded at address 0.
- A word is 10 decimal digits, stored in 10's complement, so values go from
  -5 000 000 000 to 4 999 999 999. Arithmetic wraps around.
- Registers `R0` to `R9`. `R9` (also called `SR`) is the stack pointer. It starts at
  9000 and the stack grows down. `R0` is used for I/O.
- There is a condition code (negative, zero or positive), set by most instructions, and
  an overflow flag.

### Syntax

```
LABEL:  MNEMONIC.mode  Rr, operand    | comment
```

Everything is case-insensitive, and comments start with `|`. Only the first three
letters of a mnemonic count.

The mode says how the operand is used:

| mode | meaning                                        | example         |
| ---- | ---------------------------------------------- | --------------- |
| `.w` | the number itself                              | `HIA.w R1, 5`   |
| `.a` | the address as a number                        | `HIA.a R0, MSG` |
| `.d` | the word at that address (default)             | `HIA R1, X`     |
| `.i` | the word at the address stored at that address | `HIA.i R1, P`   |

An operand can be indexed with a register: `TAB(R1)`, plus `(R1+)` and `(R1-)` to
increment or decrement afterwards, and `(+R1)` and `(-R1)` to do it first.

Data is just numbers or strings separated by `;`, and `RESGR n` reserves `n` zero
words:

```
TABLE:  12; -4; 30
MSG:    "Hello\n"
BUF:    RESGR 10
```

### Instructions

| instruction      | does                                                 |
| ---------------- | ---------------------------------------------------- |
| `HIA Rr, op`     | load: `Rr := op`                                     |
| `BIG Rr, addr`   | store: `mem[addr] := Rr`                             |
| `OPT Rr, op`     | `Rr := Rr + op`                                      |
| `AFT Rr, op`     | `Rr := Rr - op`                                      |
| `VER Rr, op`     | `Rr := Rr * op`                                      |
| `DEL Rr, op`     | `Rr := Rr / op`, rounded toward zero                 |
| `MOD Rr, op`     | `Rr := Rr mod op`, never negative                    |
| `VGL Rr, op`     | compare `Rr` with `op`, only sets the condition code |
| `SPR addr`       | jump                                                 |
| `VSP cond, addr` | jump if `cond` holds                                 |
| `SBR addr`       | call a subroutine (return address is pushed)         |
| `KTG`            | return                                               |
| `BST Rr`         | push `Rr`                                            |
| `HST Rr`         | pop into `Rr`                                        |
| `LEZ`            | read a number into `R0`                              |
| `DRU`            | print `R0`, followed by a tab                        |
| `DRS`            | print the string at address `R0`                     |
| `NWL`            | print a newline                                      |
| `NTS`            | do nothing                                           |
| `STP`            | stop                                                 |

Conditions for `VSP`, usually right after a `VGL`:

| condition | alias  | jumps when       |
| --------- | ------ | ---------------- |
| `GEL`     | `NUL`  | equal            |
| `NGEL`    | `NNUL` | not equal        |
| `GR`      | `POS`  | greater          |
| `GRG`     | `NNEG` | greater or equal |
| `KL`      | `NEG`  | less             |
| `KLG`     | `NPOS` | less or equal    |
| `OVL`     |        | overflow         |
| `GOVL`    |        | no overflow      |

`EINDPR` marks the end of the program and is optional.

### Example

Adds up a table and prints the total:

```
        HIA.w   R0, 0           | R0 = total
        HIA.w   R1, 0           | R1 = index
LOOP:   VGL.w   R1, 5
        VSP     GEL, DONE
        OPT     R0, TABLE(R1+)  | total += TABLE[index++]
        SPR     LOOP
DONE:   DRU
        NWL
        STP

TABLE:  12; -4; 30; 7; 100
```

There are a few more in `examples/`.
