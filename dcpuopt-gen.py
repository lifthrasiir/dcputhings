values = {
    # value: (code, sp delta, pc delta or "next word" flag)
     0: ('mem[%(sp)s]', 1, 0),
     1: ('mem[%(sp)s]', 0, 0),
     2: ('mem[%(sp1)s]', -1, 0),
     3: ('sp', 0, 0),
     4: ('pc', 0, 0),
     5: ('of', 0, 0),
     6: ('mem[mem[%(pc)s]]', 0, 1),
     7: ('mem[%(pc)s]', 0, 1),
     8: ('reg[(c>>%(shift)d)&7]', 0, 0),
     9: ('mem[reg[(c>>%(shift)d)&7]]', 0, 0),
    10: ('mem[(word)(reg[(c>>%(shift)d)&7]+mem[%(pc)s])]', 0, 1),
    11: ('((c>>%(shift)d)&31)', 0, 0),
}

opcodes = {
    # opcode: (code, cycles),
     1: ('%(a)s=%(b)s;', 1),
     2: ('%(acache)st=(xword)%(aref)s+(xword)%(b)s;%(aref)s=t;of=t>>16;', 2),
     3: ('%(acache)st=(xword)%(aref)s-(xword)%(b)s;%(aref)s=t;of=t>>16;', 2),
     4: ('%(acache)st=(xword)%(aref)s*(xword)%(b)s;%(aref)s=t;of=t>>16;', 3),
     5: ('%(acache)su=%(b)s;t=(u?((xword)%(aref)s<<16)/u:0);%(aref)s=t>>16;of=t;', 3),
     6: ('%(acache)su=%(b)s;%(aref)s=(u?(xword)%(aref)s%%u:0);', 3),
     7: ('%(acache)su=%(b)s;t=(u>31?0:(xword)%(aref)s<<u);%(aref)s=t;of=t>>16;', 2),
     8: ('%(acache)su=%(b)s;t=(u>31?0:(xword)%(aref)s<<16>>u);%(aref)s=t>>16;of=t;', 2),
     9: ('%(a)s&=%(b)s;', 1),
    10: ('%(a)s|=%(b)s;', 1),
    11: ('%(a)s^=%(b)s;', 1),
    12: ('sk=(%(a)s!=%(b)s);', 1),
    13: ('sk=(%(a)s==%(b)s);', 1),
    14: ('sk=(%(a)s<=%(b)s);', 1),
    15: ('sk=((%(a)s&%(b)s)!=0);', 1),
}

lines = {}
for op in xrange(1, 16):
    opcode, opcycles = opcodes[op]
    opskip = (op >= 12)
    for a in xrange(12):
        acode, asp, apc = values[a]
        acode = acode % {'sp': 'sp', 'sp1': '(word)(sp-1)', 'pc': 'pc', 'shift': 4}
        # only applicable for certain codes
        if a in (6, 8, 9, 10):
            acache = 'v=%s;' % acode[4:-1]
            aref = '%sv%s' % (acode[:4], acode[-1:])
        else:
            acache = ''
            aref = acode
        ajump = (a == 4)
        alit = a in (10, 11)
        for b in xrange(12):
            bcode, bsp, bpc = values[b]
            bcode = bcode % {'sp': '(word)(sp%+d)'%asp if asp else 'sp',
                             'sp1': '(word)(sp%+d)'%(asp-1) if asp-1 else 'sp',
                             'pc': '(word)(pc%+d)'%apc if apc else 'pc', 'shift': 10}
            bcode = bcode.replace('(word)(sp+1)-1', 'sp')
            if opskip or not alit:
                code = opcode % {'a': acode, 'acache': acache, 'aref': aref, 'b': bcode}
            else:
                # if it is not a branch instruction
                # and the lhs is a literal
                # then the assignment does not occur
                # but all the side effect remains.
                code = ''
            if asp + bsp < 0:
                code += 'sp-=%d;' % -(asp + bsp)
            elif asp + bsp > 0:
                code += 'sp+=%d;' % (asp + bsp)
            if not ajump: # SET PC, ... ignores all side effects on PC
                if apc + bpc:
                    if opskip:
                        code += 'pc+=%d;if(sk)pc+=length(mem[pc]);' % (apc + bpc)
                    else:
                        code += 'pc+=%d;' % (apc + bpc)
                elif opskip:
                    code += 'if(sk)pc+=length(mem[pc]);'
            if opskip:
                code += 'wc+=%d+sk;' % (apc + bpc + opcycles)
            else:
                code += 'wc+=%d;' % (apc + bpc + opcycles)
            icode = (b*12+a)*16+op
            lines[icode] = 'case %d: %sbreak;' % (icode, code)
for k, v in sorted(lines.items()):
    print v

