# dcupthings

This is the **dcputhings**, assorted tools for DCPU-16 development. This
repository is maintained by [Kang Seonghoon](http://mearie.org/) and contains
the following softwares:

* `dcpu.c` and `dcpuopt.c`, my early attempts to build a DCPU-16 emulator.
* DcpuAsm, an Ocaml DSL for DCPU-16 assembly.


## DcpuAsm

DcpuAsm is a DCPU-16 assembler *embedded in the Ocaml syntax*. It allows
easier DCPU-16 code generation, but it can also be used as an ordinary macro
assembler if you understand a bit of Ocaml.

Basically, the assembly is represented as an Ocaml list:

    [
        SET A, 4;
        SET B, 5;
    %loop:
        SET PC, %loop;
    ]

More specifically, `SET A, 4`, `SET B, 5` and `%loop: SET PC, %loop` evaluates
to the internal representation via Camlp4. Note that Ocaml allows the trailing
semicolon in the brackets, so the last `;` is just fine.

This list can be converted to the binary via `ASM` keyword:

    let code = ASM [
        SET A, 4;
        SET B, 5;
    %loop:
        SET PC, %loop;
    ];;
    print_string (DcpuAsm.to_binary_le code)

This will resolve all the labels to the fixed offset. `to_binary_le` converts
the static code into the little-endian byte string. The big-endian counterpart
is `to_binary_be`, and you can get an array of words using `to_words`.

By default `ASM` assumes the origin at 0x0000. This can be changed using `ASM
~origin:0x1000 [...]` syntax; in fact, `ASM` is just a shorter alias to
`DcpuAsm.asm` function.

### Statements / Instructions

DcpuAsm supports the following instructions (and pseudo-instructions):

* Basic opcodes: `SET`, `ADD`, `SUB`, `MUL`, `DIV`, `MOD`, `SHL`, `SHR`,
  `AND`, `BOR`, `XOR`, `IFE`, `IFN`, `IFG`, `IFB`.
* Non-basic opcodes: `JSR`
* Raw data: `DAT`
* Syntactic extensions: `NOP`, `JMP`, `PUSH`, `POP`, `BRK`/`HLT`
* Empty opcode (i.e. no output at all): `PASS`

Basic opcodes has two arguments, and non-basic opcodes has one of them.
Multiple arguments are separated with `,` as much like other assemblers.

`DAT` has one or more arguments. The argument can be a typical immediate (see
below for the syntax) which occupies exactly one word, or a string which
occupies the same number of words (so that `"ok"` equals to `'o', 'k'`).

`NOP` is equivalent to `SET A, A`. It does nothing but take one cycle. While
DCPU-16 has lots of nops, this encoding is chosen because of the simplicity of
its binary encoding (0x0001). It may change if 0x0000 also turns out to be a
nop.

`JMP a` is equivalent to `SET PC, a`.

`PUSH a` is equivalent to `SET PUSH, a`. `POP a` is equivalent to `SET a,
POP`. You can also use `[SP]` instead of `PEEK`. (But `[SP+<number>]` is still
invalid.)

`BRK` or `HLT` is equivalent to `SUB PC, 1`. This forms a simple infinite
loop, and used as a *de facto* instruction to terminate the emulator.

`PASS` does not emit the binary at all; it can be used as a placeholder.

DcpuAsm does not support `EQU` pseudo-instruction or similar; you can use an
ordinary `let x = ... in ...` construct to define constants, however.

### Labels

DcpuAsm supports labels. Labels are a valid Ocaml name (always starts with a
lowercase letter or `_`) prepended by `%`; `%asdf`, `%_foo_bar`, `%loop42`
are valid labels, for example.

There are two ways to use labels:

1. It can occur in the expression, and evaluates to the location pointed by
   the label. The instruction may contain labels defined after it.
2. It can also occur at the front of the instruction (e.g. `%foo: SET A, 3`)
   to declare the label. The colon (`:`) is optional for predefined
   instructions, but you are recommended to keep the colon as it allows
   multiple label definitions. Skipping a colon may be natural for `DAT`
   instructions however.

You can define labels at the end of list; the `PASS` statement will be
implicitly added:

    [
        JMP %garbage;
        DAT 1, 2, 3, 4;
    %garbage:
    ]

DcpuAsm will automatically resolve labels to the appropriate position. Since
the length of instructions may vary depending on the position of labels,
DcpuAsm runs multiple passes to settle them down. If it is not stabilized
after given number of passes DcpuAsm gives up. The default limit is 50, but
can be configured like `ASM ~maxpass:10 [...]`.

It is possible to have free (undefined) labels in the assembly. DcpuAsm will
make sure that these labels, while unresolved, will not affect the other parts
of generated code. This is done by forcing all remaining immediates to always
use a longer form.

It is advised to prepend `_` to local labels. DcpuAsm has a special support
for these local labels (see below).

The special label `%_`, when used in the expression, resolves to the position
of the current instruction. For example `SET PC, %_` will be same as `%_temp:
SET PC, %_temp`. You cannot define a label named `%_`.

### Expressions

Expression can occur as an instruction's argument. It may contain registers,
numbers, labels, memory references (enclosed in `[]`) and expressions.

DcpuAsm supports all general and special registers: `A`, `B`, `C`, `X`, `Y`,
`Z`, `I`, `J`, `SP`, `PC`, `O`. It also supports `PUSH`, `PEEK` and `POP`;
they cannot be used in the expression.

DcpuAsm supports numbers in base 2 (`0b101`), base 8 (`0o337`), base 10 and
base 16 (`0x1337`) just like Ocaml. Additionally a character literal (`'A'`)
will be equal to its numerical code (i.e. `int_of_char 'A'`). All numbers are
treated as built-in Ocaml numbers (31 or 63 bits long depending on the
platform) so you should be aware of it.

DcpuAsm supports all ordinary arithmetic and bitwise operations: `+`, `-`,
`*`, `DIV`, `MOD`, `NOT`, `AND`, `OR`, `XOR`, `SHL`, `SHR`. While arithmetic
operations are permitted for registers, the resulting expression has to be in
the form `register + other expression` or `[register + other expression]` due
to the constraint of DCPU-16. (The intermediate expression does *not* have to
however: `[3*A+2*(2*B-A)-(8 DIV 2)*B]` will be resolved to `[A]`, which is
perfectly valid in DCPU-16.)

As mentioned before, labels in the expression evaluate to their positions. You
can do something like this:

    [
    (* Returns sqrt(A) from the precomputed table. A should be less than 16.
     * B will contain an integral part and A will contain a fractional part.
     *)
    %isqrt:
        IFG A, (%_fpend-%_fpstart) DIV 2 - 1; (* bound check *)
            HLT;
        MUL A, 2;
        SET B, [%_fpstart+A];
        SET A, [%_fpstart+A+1];
        JMP POP;
    %_fpstart:
        DAT 0x0000, 0x0000; DAT 0x0001, 0x0000;
        DAT 0x0001, 0x6a0a; DAT 0x0001, 0xbb68;
        DAT 0x0002, 0x0000; DAT 0x0002, 0x3c6f;
        DAT 0x0002, 0x7312; DAT 0x0002, 0xa550;
        DAT 0x0002, 0xd414; DAT 0x0003, 0x0000;
        DAT 0x0003, 0x298b; DAT 0x0003, 0x510e;
        DAT 0x0003, 0x76cf; DAT 0x0003, 0x9b05;
        DAT 0x0003, 0xbddd; DAT 0x0003, 0xdf7c;
    %_fpend:
    ]

`IMM (e)` (note the parentheses) and `PTR [e]` is a longer form of `e` and
`[e]`, respectively, and you should use them outside the assembly instruction.

Normal Ocaml expression can also be used; `VAL e` will evaluate `e` as an
Ocaml expression and use its value as an immediate. Similarly, `STR e` uses
its value as a string (only useful in `DAT` arguments). If the Ocaml
expression is simple enough (e.g. a single identifier) then you can omit
`VAL` entirely. This is very useful for compile-time constants:

    let screen_base = 0x8000 in
    [
        SET [screen_base], 'H';
        SET [screen_base+1], 'e';
        SET [screen_base+2], 'l';
        SET [screen_base+3], 'l';
        SET [screen_base+4], 'o';
        HLT;
    ]

DcpuAsm tries to generate the shortest code for given assembly, but you can
override this behavior by `SHORT` and `LONG` prefixes. `SHORT e` will cause an
error when `e` does not fit in the range of 0--31, and `LONG e` will generate
a longer form of given immediate (not the instruction, so should use it twice
for basic opcodes). This only applies to a literal value; it is silently
ignored in other kind of values.

### Blocks

DcpuAsm supports a block as a unit of assembly instructions. They can be used
as a building block:

    (* Warning: not tail-recursive. Illustration purpose only. *)
    let copyn src dst n =
        if n = 0 then
            PASS
        else if n = 1 then
            SET [dst], [src]
        else
            BLOCK [
                SET [dst], [src];
                copyn (src+1) (dst+1) (n-1);
            ]
    in
    [
        (* save and restore the video memory *)
        copyn 0x8000 0x4000 (16*32);
        copyn 0x4000 0x8000 (16*32);
        HLT;
    ]

`BLOCK e` evaluates `e` as an *Ocaml* expression (which includes,
incidentally, a list containing assembly instructions) and makes an
instruction block out of it. You don't have use blocks if you have exactly one
instruction, as the case `n = 1` of the above code suggests.

More interesting use of blocks involves local labels:

    let case k = BLOCK [ (* ... *) ] in
    [
        BLOCK LOCAL [
            IFE A, 1;
                JMP %_next;
            JMP %_skip;
        %_next:
            case 1;
        %_skip:
        ];
        BLOCK LOCAL [
            IFE A, 2;
                JMP %_next;
            JMP %_skip;
        %_next:
            case 2;
        %_skip:
        ];
        HLT;
    ]

`BLOCK LOCAL` will make all defined labels starting with `_` local. It is not
possible to access these local labels outside of the block (unless you use a
nasty hack). Non-local blocks (in this case, `case 1` and `case 2`) will not
affect this procedure. This is very useful for generated codes.

You can manually give a list of local labels using `BLOCK LOCAL %a, %b, %c`
syntax; or by an Ocaml list using `BLOCK LOCAL *["a"; "b"; "c"]`.
`DcpuAsmExample.ml` contains some extreme example of local blocks.

### Macros

There are no separate macro feature in DcpuAsm, but you can trivially make a
simple macro with local blocks and Ocaml `let` construct.

DcpuAsm *does* support some additional features for macros. While `VAL` and
`STR` allows the insertion of arbitrary immediate value or string, you cannot
insert registers or other expression in this way. Therefore DcpuAsm supports a
quoted form `#e` of an Ocaml expression, which can appear in:

* Expressions (e.g. `3 + #reg`). The expression should evaluate to the
  internal representation of expression; an immediate should be quoted using
  `IMM` prefix.
* Labels (e.g. `%#labelname`). The expression should evaluate to a string.
  While you can use any character in the label name (even an empty string is
  permitted), you should restrict yourself to the normal identifier. Local
  labels, for example, start with `.` character internally. `DcpuAsm.gensym`
  function can be used to generate unique symbols.

### Caveats / To-do List

As always you should expect the following caveats:

* You cannot use codes like `[(%label1, ...); (%label2, ...); ...]` in the
  normal Ocaml code because `(%` will be treated as one token. (`( %label1`
  etc. will work.) The assembly syntax is specially crafted to separate those
  two, however. Any suggestions about this problem are welcomed.
* `JMP` does not try to generate an optimal code (e.g. using `ADD` or `SUB`)
  yet.
* `TIMES` prefix and `?` in the `DAT` pseudo-instruction is missing yet.
* `DAT` is missing `*`-prefixed items.

