/*
DCPU-16 emulator in C. Horribly inefficient.
Written by Kang Seonghoon (http://mearie.org/, @senokay). Public Domain.
*/

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef uint16_t word;
typedef uint32_t xword;

#define CASEx8(x) \
	case (x)+0: case (x)+1: case (x)+2: case (x)+3: \
	case (x)+4: case (x)+5: case (x)+6: case (x)+7

int wc;
word pc, sp, of, reg[8], mem[1<<16];

void init(void)
{
	wc = 0;
	pc = 0x0000;
	sp = 0x0000; // well, the first PUSH is made to 0xffff, so...
	of = 0x0000;
	memset(reg, 0, sizeof reg);
}

// sink is used for literals, which of course do not have an address but
// we need it anyway for consistency. the sink is discarded later.
word *get(word v, word *sink)
{
	switch (v) {
	CASEx8(0x00):		return &reg[v];
	CASEx8(0x08):		return &mem[reg[v-0x08]];
	CASEx8(0x10):	++wc;	return &mem[reg[v-0x10] + mem[pc++]];
	case 0x18:		return &mem[sp++];
	case 0x19:		return &mem[sp];
	case 0x1a:		return &mem[--sp];
	case 0x1b:		return &sp;
	case 0x1c:		return &pc;
	case 0x1d:		return &of;
	case 0x1e:	++wc;	return &mem[mem[pc++]];
	case 0x1f:	++wc;	*sink = mem[pc++]; return sink; // literal
	CASEx8(0x20):
	CASEx8(0x28):
	CASEx8(0x30):
	CASEx8(0x38):		*sink = v-0x20; return sink; // literal
	default:		abort();
	}
}

void tick(void)
{
	word c = mem[pc++];
	word op = c & 15, aa = (c >> 4) & 63, bb = c >> 10;

	if (op) { // typical opcodes
		word a0, b0;
		xword tmp;
		word *a = get(aa, &a0);
		word *b = get(bb, &b0);

		switch (op) {
		case 0x01: // SET
			*a = *b;
			wc += 1;
			break;

		case 0x02: // ADD
			tmp = (xword)*a + (xword)*b;
			of = (tmp >= 0x10000 ? 1 : 0);
			*a = (word)(tmp & 0xffff);
			wc += 2;
			break;

		case 0x03: // SUB
			tmp = (xword)*a + (xword)~*b + 1;
			of = (tmp >= 0x10000 ? 0xffff : 0);
			*a = (word)(tmp & 0xffff);
			wc += 2;
			break;

		case 0x04: // MUL
			tmp = (xword)*a * (xword)*b;
			of = (word)(tmp >> 16);
			*a = (word)(tmp & 0xffff);
			wc += 2;
			break;

		case 0x05: // DIV
			tmp = (*b ? ((xword)*a << 16) / (xword)*b : 0);
			of = (word)(tmp & 0xffff);
			*a = (word)(tmp >> 16);
			wc += 3;
			break;

		case 0x06: // MOD
			*a = (*b ? *a % *b : 0);
			wc += 3;
			break;

		case 0x07: // SHL
			tmp = (*b >= 32 ? 0 : (xword)*a << (xword)*b);
			of = (word)(tmp >> 16);
			*a = (word)(tmp & 0xffff);
			wc += 2;
			break;

		case 0x08: // SHR
			tmp = (*b >= 32 ? 0 : ((xword)*a << 16) >> (xword)*b);
			of = (word)(tmp & 0xffff);
			*a = (word)(tmp >> 16);
			wc += 2;
			break;

		case 0x09: // AND
			*a = *a & *b;
			wc += 1;
			break;

		case 0x0a: // BOR
			*a = *a | *b;
			wc += 1;
			break;

		case 0x0b: // XOR
			*a = *a ^ *b;
			wc += 1;
			break;

		case 0x0c: // IFE
			if (*a == *b) {
				wc += 1;
			} else {
				pc += 1;
				wc += 2;
			}
			break;

		case 0x0d: // IFN
			if (*a != *b) {
				wc += 1;
			} else {
				pc += 1;
				wc += 2;
			}
			break;

		case 0x0e: // IFG
			if (*a > *b) {
				wc += 1;
			} else {
				pc += 1;
				wc += 2;
			}
			break;

		case 0x0f: // IFB
			if (*a & *b) {
				wc += 1;
			} else {
				pc += 1;
				wc += 2;
			}
			break;

		default:
			abort();
		}
	} else if (aa) {
		word b0;
		switch (aa) {
		case 0x01: // JSR
			mem[--sp] = pc;
			pc = *get(bb, &b0);
			wc += 2;
			break;

		default:
			if (aa >= 0x02 && aa <= 0x3f) {
				fprintf(stderr, "Reserved opcode %d:0\n", (int)aa);
				wc += 1;
				break;
			}
			abort();
		}
	} else {
		fprintf(stderr, "Reserved opcode %d:0:0\n", (int)bb);
		wc += 1;
	}
}

int dumpv(word v, word pc)
{
	static const char ops[8] = "ABCXYZIJ";

	switch (v) {
	CASEx8(0x00):	putchar(ops[v]); return 0;
	CASEx8(0x08):	printf("[%c]", ops[v-0x08]); return 0;
	CASEx8(0x10):	printf("[%c%+d]", ops[v-0x10], (int)mem[pc]); return 1;
	case 0x18:	printf("[SP++]"); return 0;
	case 0x19:	printf("[SP]"); return 0;
	case 0x1a:	printf("[--SP]"); return 0;
	case 0x1b:	printf("SP"); return 0;
	case 0x1c:	printf("PC"); return 0;
	case 0x1d:	putchar('O'); return 0;
	case 0x1e:	printf("[%#x]", (int)mem[pc]); return 1;
	case 0x1f:	printf("%#x", (int)mem[pc]); return 1;
	CASEx8(0x20):
	CASEx8(0x28):
	CASEx8(0x30):
	CASEx8(0x38):	printf("%d", (int)(v-0x20)); return 0;
	default:	abort();
	}
}

word dump(word pc)
{
	word oldpc = pc;
	word c = mem[pc++];
	word op = c & 15, a = (c >> 4) & 63, b = c >> 10;

	printf("%04x: ", oldpc);
	if (op) {
		static const char opcodes[16][4] = {
			"???", "SET", "ADD", "SUB", "MUL", "DIV", "MOV", "SHL",
			"SHR", "AND", "BOR", "XOR", "IFE", "IFN", "IFG", "IFB",
		};
		printf("%s ", opcodes[op]);
		pc += dumpv(a, pc);
		printf(", ");
		pc += dumpv(b, pc);
		printf("\n");
	} else {
		switch (a) {
		case 0x00:
			printf("; Reserved opcode %d:0:0\n", (int)b);
			break;
		case 0x01:
			printf("JSR ");
			pc += dumpv(b, pc);
			printf("\n");
			break;
		default:
			if (a >= 0x02 && a <= 0x3f) {
				printf("; Reserved opcode %d:0\n", (int)a);
				break;
			}
			abort();
		}
	}
	return pc - oldpc;
}

void stat(void)
{
	printf("PC=%04x SP=%04x O=%04x Reg=%04x:%04x:%04x:%04x:%04x:%04x:%04x:%04x (t=%d)\n",
		pc, sp, of, reg[0], reg[1], reg[2], reg[3], reg[4], reg[5], reg[6], reg[7], wc);
	dump(pc);
}

int main(void) {
	word initmem[] = {
		0x7c01,0x0030,0x7de1,0x1000,0x0020,0x7803,0x1000,0xc00d,
		0x7dc1,0x001a,0xa861,0x7c01,0x2000,0x2161,0x2000,0x8463,
		0x806d,0x7dc1,0x000d,0x9031,0x7c10,0x0018,0x7dc1,0x001a,
		0x9037,0x61c1,0x7dc1,0x001a,0x0000,0x0000,0x0000,0x0000,
	};
	int i;

	for (i = 0; i < (int)(sizeof(initmem) / sizeof(*initmem)); ++i) {
		mem[i] = initmem[i];
	}

	init();
	while (wc < 100) {
		stat();
		tick();
	}

	return 0;
}

