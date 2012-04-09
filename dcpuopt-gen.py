# strategy:
# we first generate (somewhat) verbose code and remove redundant "+0"s.

values = {
    # value: (code, sp delta, pc delta or "next word" flag)
     0: ('mem[sp%(spdelta)+d]', 1, 0),
     1: ('mem[sp%(spdelta)+d]', 0, 0),
     2: ('mem[sp%(spdelta)+d-1]', -1, 0),
     3: ('sp', 0, 0),
     4: ('pc', 0, 0),
     5: ('of', 0, 0),
     6: ('mem[mem[pc%(pcdelta)+d]]', 0, 1),
     7: ('mem[pc%(pcdelta)+d]', 0, 1),
     8: ('reg[(c>>%(shift)d)&7]', 0, 0),
     9: ('mem[reg[(c>>%(shift)d)&7]]', 0, 0),
    10: ('mem[reg[(c>>%(shift)d)&7]+mem[pc%(pcdelta)+d]]', 0, 1),
    11: ('((c>>%(shift)d)&31)', 0, 0),
}

opcodes = {
    # opcode: (code, cycles),
     1: ('%(a)s=%(b)s;', 1),
     2: ('%(acache)st=(xword)%(aref)s+(xword)%(b)s;of=t>>16;%(aref)s=t;', 2),
     3: ('%(acache)st=(xword)%(aref)s-(xword)%(b)s;of=t>>16;%(aref)s=t;', 2),
     4: ('%(acache)st=(xword)%(aref)s*(xword)%(b)s;of=t>>16;%(aref)s=t;', 3),
     5: ('%(acache)su=%(b)s;t=(u?((xword)%(aref)s<<16)/u:0);of=t;%(aref)s=t>>16;', 3),
     6: ('%(acache)su=%(b)s;%(aref)s=(u?(xword)%(aref)s%%u:0);', 3),
     7: ('%(acache)su=%(b)s;t=(u>31?0:(xword)%(aref)s<<u);of=t>>16;%(aref)s=t;', 2),
     8: ('%(acache)su=%(b)s;t=(u>31?0:(xword)%(aref)s<<16>>u);of=t;%(aref)s=t>>16;', 2),
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
        acode = acode % {'spdelta': 0, 'pcdelta': 0, 'shift': 4}
        acode = acode.replace('+0', '').replace('+1-1', '')
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
            bcode = bcode % {'spdelta': asp, 'pcdelta': apc, 'shift': 10}
            bcode = bcode.replace('+0', '').replace('+1-1', '')
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
                        code += 'pc+=%d+sk;' % (apc + bpc)
                    else:
                        code += 'pc+=%d;' % (apc + bpc)
                elif opskip:
                    code += 'pc+=sk;'
            if opskip:
                code += 'wc+=%d+sk;' % (apc + bpc + opcycles)
            else:
                code += 'wc+=%d;' % (apc + bpc + opcycles)
            icode = (b*12+a)*16+op
            lines[icode] = 'case %d: %sbreak;' % (icode, code)
for k, v in sorted(lines.items()):
    print v

