(* dcputhings: Assorted Tools for DCPU-16 Development.
 * Written by Kang Seonghoon. See LICENSE for the full license statement.
 *)

module Id = struct
    let name = "pa_dcpuasm"
    let version = "1.0"
end

open Camlp4

module Make (Syntax : Sig.Camlp4Syntax) = struct
    open Sig
    include Syntax

    class label_extractor = object (self)
        inherit Ast.fold as super

        val defined = Hashtbl.create 4
        val used = Hashtbl.create 4

        method defined = defined
        method used = used

        method expr = function
            | <:expr< DcpuAsm.Stmt.label $str:label$ >> ->
              Hashtbl.replace defined label (); self
            | <:expr< DcpuAsm.Expr.label $str:label$ >> ->
              Hashtbl.replace used label (); self
            | e -> super#expr e
    end

    let extract_labels e =
        let ex = (new label_extractor)#expr e in
        let defined = Hashtbl.fold (fun k v t -> k::t) ex#defined [] in
        let used = Hashtbl.fold (fun k v t -> k::t) ex#used [] in
        (defined, used)

    let list_of_locally_defined_labels _loc ss =
        let defined, _ = extract_labels ss in
        let folder acc s =
            if String.length s > 0 && s.[0] = '_' then
                <:expr< $str:s$ :: $acc$ >>
            else
                <:expr< $acc$ >>
        in
        List.fold_left folder <:expr< [] >> defined

    EXTEND Gram
        GLOBAL: a_LIDENT a_STRING a_INT a_CHAR expr;

        asmbarelabel:
        [
            [ label = a_LIDENT ->
              <:expr< DcpuAsm.Expr.label $str:label$ >>
            | "_" ->
              <:expr< DcpuAsm.Expr.label "_" >>
            | "#"; e = expr LEVEL "simple" ->
              <:expr< DcpuAsm.Expr.label $e$ >>
            ]
        ];

        asmlabel:
        [
            [ "%"; l = asmbarelabel -> <:expr< $l$ >> ]
        ];

        asmlabelstr:
        [
            [ "%"; l = a_LIDENT -> <:expr< $str:l$ >>
            | "%"; "_" -> raise (Stream.Error "%_ cannot be defined")
            | "%"; "#"; e = expr LEVEL "simple" -> <:expr< $e$ >>
            ]
        ];

        asmlabelstrlist:
        [
            [ l = asmlabelstr; OPT "," -> <:expr< [$l$] >>
            | l = asmlabelstr; OPT ","; ls = SELF -> <:expr< $l$::$ls$ >>
            ]
        ];

        asmreg: (* only one that can be used in memory references *)
        [
            [ "A" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.A >>
            | "B" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.B >>
            | "C" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.C >>
            | "X" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.X >>
            | "Y" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.Y >>
            | "Z" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.Z >>
            | "I" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.I >>
            | "J" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.J >>
            | "SP" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.SP >>
            ]
        ];

        asmexpr0: (* same as asmexpr but assumes that it starts with % *)
        [
            "top"
            [] (* level is present, but no rule matches here *)
        |
            "+" LEFTA
            [ e1 = SELF; "+"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.Expr.add $e1$ $e2$ >>
            | e1 = SELF; "+%"; e2 = asmexpr0 LEVEL "*" ->
              <:expr< DcpuAsm.Expr.add $e1$ $e2$ >>
            | e1 = SELF; "-"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.Expr.sub $e1$ $e2$ >>
            | e1 = SELF; "-%"; e2 = asmexpr0 LEVEL "*" ->
              <:expr< DcpuAsm.Expr.sub $e1$ $e2$ >>
            | e1 = SELF; "OR"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.Expr.or_ $e1$ $e2$ >>
            | e1 = SELF; "XOR"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.Expr.xor $e1$ $e2$ >>
            ]
        |
            "*" LEFTA
            [ e1 = SELF; "*"; e2 = asmexpr LEVEL "~-" ->
              <:expr< DcpuAsm.Expr.mul $e1$ $e2$ >>
            | e1 = SELF; "*%"; e2 = asmexpr0 LEVEL "~-" ->
              <:expr< DcpuAsm.Expr.mul $e1$ $e2$ >>
            | e1 = SELF; "DIV"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.Expr.div $e1$ $e2$ >>
            | e1 = SELF; "MOD"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.Expr.mod_ $e1$ $e2$ >>
            | e1 = SELF; "AND"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.Expr.and_ $e1$ $e2$ >>
            ]
        |
            "shift" LEFTA
            [ e1 = SELF; "SHL"; e2 = asmexpr LEVEL "~-" ->
              <:expr< DcpuAsm.Expr.shl $e1$ $e2$ >>
            | e1 = SELF; "SHR"; e2 = asmexpr LEVEL "~-" ->
              <:expr< DcpuAsm.Expr.shr $e1$ $e2$ >>
            ]
        |
            "~-" NONA
            [] (* level is present, but no rule matches here *)
        |
            "asm simple"
            [] (* level is present, but no rule matches here *)
        |
            "simple"
            [ l = asmbarelabel -> <:expr< $l$ >>
            ]
        ];

        asmexpr:
        [
            "top" NONA
            [ "LONG"; e = asmexpr -> <:expr< DcpuAsm.Expr.long $e$ >>
            | "SHORT"; e = asmexpr -> <:expr< DcpuAsm.Expr.short $e$ >>
            ]
        |
            "+" LEFTA
            [ e1 = SELF; "+"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.Expr.add $e1$ $e2$ >>
            | e1 = SELF; "+%"; e2 = asmexpr0 LEVEL "*" ->
              <:expr< DcpuAsm.Expr.add $e1$ $e2$ >>
            | e1 = SELF; "-"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.Expr.sub $e1$ $e2$ >>
            | e1 = SELF; "-%"; e2 = asmexpr0 LEVEL "*" ->
              <:expr< DcpuAsm.Expr.sub $e1$ $e2$ >>
            | e1 = SELF; "OR"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.Expr.or_ $e1$ $e2$ >>
            | e1 = SELF; "XOR"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.Expr.xor $e1$ $e2$ >>
            ]
        |
            "*" LEFTA
            [ e1 = SELF; "*"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.Expr.mul $e1$ $e2$ >>
            | e1 = SELF; "*%"; e2 = asmexpr0 LEVEL "shift" ->
              <:expr< DcpuAsm.Expr.mul $e1$ $e2$ >>
            | e1 = SELF; "DIV"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.Expr.div $e1$ $e2$ >>
            | e1 = SELF; "MOD"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.Expr.mod_ $e1$ $e2$ >>
            | e1 = SELF; "AND"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.Expr.and_ $e1$ $e2$ >>
            ]
        |
            "shift" LEFTA
            [ e1 = SELF; "SHL"; e2 = asmexpr LEVEL "~-" ->
              <:expr< DcpuAsm.Expr.shl $e1$ $e2$ >>
            | e1 = SELF; "SHR"; e2 = asmexpr LEVEL "~-" ->
              <:expr< DcpuAsm.Expr.shr $e1$ $e2$ >>
            ]
        |
            "~-" NONA
            [ "-"; e = asmexpr LEVEL "asm simple" ->
              <:expr< DcpuAsm.Expr.neg $e$ >>
            | "-%"; e = asmexpr0 LEVEL "asm simple" ->
              <:expr< DcpuAsm.Expr.neg $e$ >>
            | "NOT"; e = asmexpr LEVEL "asm simple" ->
              <:expr< DcpuAsm.Expr.not_ $e$ >>
            ]
        |
            "asm simple"
            [ v = a_INT ->
              <:expr< DcpuAsm.Expr.imm $int:v$ >>
            | v = a_CHAR ->
              <:expr< DcpuAsm.Expr.imm (int_of_char $chr:v$) >>
            | "PICK"; e = asmexpr LEVEL "+" ->
              <:expr< DcpuAsm.Expr.pick $e$ >>
            | "["; e = asmexpr LEVEL "+"; "]" ->
              <:expr< DcpuAsm.Expr.mem $e$ >>
            | "[%"; e = asmexpr0 LEVEL "+"; "]" ->
              <:expr< DcpuAsm.Expr.mem $e$ >>
            | "("; e = asmexpr LEVEL "+"; ")" -> <:expr< $e$ >>
            | "(%"; e = asmexpr0 LEVEL "+"; ")" -> <:expr< $e$ >>
            | "#"; e = expr LEVEL "simple" -> <:expr< $e$ >>
            | "_" -> <:expr< DcpuAsm.Expr.blank >>
            | i = val_longident -> <:expr< DcpuAsm.Expr.imm $id:i$ >>
            ]
        |
            "simple" NONA
            [ l = asmlabel -> <:expr< $l$ >> (* CAUTION! *)
            | r = asmreg -> <:expr< $r$ >>
            | "PC" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.PC >>
            | "EX" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.EX >>
            | "O" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.EX >> (* compat. *)
            | "IA" -> <:expr< DcpuAsm.Expr.reg DcpuAsm.IA >>
            | "PUSH" -> <:expr< DcpuAsm.Expr.push >>
            | "POP" -> <:expr< DcpuAsm.Expr.pop >>
            | "PEEK" -> <:expr< DcpuAsm.Expr.peek >>
            | "NEXT" -> <:expr< DcpuAsm.Expr.next >>
            | "VAL"; e = expr LEVEL "simple" ->
              <:expr< DcpuAsm.Expr.imm $e$ >>
            | "STR"; e = expr LEVEL "simple" ->
              <:expr< DcpuAsm.Expr.str $e$ >>
            | "PTR"; "["; e = asmexpr LEVEL "+"; "]" ->
              <:expr< DcpuAsm.Expr.mem $e$ >>
            | "PTR"; "[%"; e = asmexpr0 LEVEL "+"; "]" ->
              <:expr< DcpuAsm.Expr.mem $e$ >>
            | "IMM"; "("; e = asmexpr LEVEL "+"; ")" -> <:expr< $e$ >>
            | "IMM"; "(%"; e = asmexpr0 LEVEL "+"; ")" -> <:expr< $e$ >>
            ]
        ];

        asmoperand:
        [
            [ e = asmexpr -> <:expr< $e$ >>
            | e = expr LEVEL "simple" -> <:expr< $e$ >>
            ]
        ];

        asmdatum:
        [
            [ cnt = asmoperand; "TIMES"; s = a_STRING ->
              <:expr< DcpuAsm.Expr.times $cnt$
                                              (DcpuAsm.Expr.str $str:s$) >>
            | cnt = asmoperand; "TIMES"; e = asmoperand ->
              <:expr< DcpuAsm.Expr.times $cnt$ $e$ >>
            | s = a_STRING ->
              <:expr< DcpuAsm.Expr.str $str:s$ >>
            | e = asmoperand ->
              <:expr< $e$ >>
            ]
        ];

        asmdata:
        [
            [ ","; datum = asmdatum; tail = asmdata ->
              <:expr< $datum$ :: $tail$ >>
            | OPT "," ->
              <:expr< [] >>
            ]
        ];

        asmstmt:
        [
            [ "DAT"; datum = asmdatum; tail = asmdata ->
              <:expr< DcpuAsm.Stmt.dat ($datum$ :: $tail$) >>
            | "SET"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.set $a$ $b$ >>
            | "ADD"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.add $a$ $b$ >>
            | "SUB"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.sub $a$ $b$ >>
            | "MUL"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.mul $a$ $b$ >>
            | "MLI"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.mli $a$ $b$ >>
            | "DIV"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.div $a$ $b$ >>
            | "DVI"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.dvi $a$ $b$ >>
            | "MOD"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.mod_ $a$ $b$ >>
            | "MDI"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.mdi $a$ $b$ >>
            | "AND"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.and_ $a$ $b$ >>
            | "BOR"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.bor $a$ $b$ >>
            | "XOR"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.xor $a$ $b$ >>
            | "SHR"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.shr $a$ $b$ >>
            | "ASR"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.asr_ $a$ $b$ >>
            | "SHL"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.shl $a$ $b$ >>
            | "IFB"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.ifb $a$ $b$ >>
            | "IFC"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.ifc $a$ $b$ >>
            | "IFE"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.ife $a$ $b$ >>
            | "IFN"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.ifn $a$ $b$ >>
            | "IFG"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.ifg $a$ $b$ >>
            | "IFA"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.ifa $a$ $b$ >>
            | "IFL"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.ifl $a$ $b$ >>
            | "IFU"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.ifu $a$ $b$ >>
            | "ADX"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.adx $a$ $b$ >>
            | "SBX"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.sbx $a$ $b$ >>
            | "STI"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.sti $a$ $b$ >>
            | "STD"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Stmt.std $a$ $b$ >>
            | "JSR"; a = asmoperand ->
              <:expr< DcpuAsm.Stmt.jsr $a$ >>
            | "HCF"; a = asmoperand ->
              <:expr< DcpuAsm.Stmt.hcf $a$ >>
            | "INT"; a = asmoperand ->
              <:expr< DcpuAsm.Stmt.int_ $a$ >>
            | "IAG"; a = asmoperand ->
              <:expr< DcpuAsm.Stmt.iag $a$ >>
            | "IAS"; a = asmoperand ->
              <:expr< DcpuAsm.Stmt.ias $a$ >>
            | "IAP"; a = asmoperand ->
              <:expr< DcpuAsm.Stmt.iap $a$ >>
            | "IAQ"; a = asmoperand ->
              <:expr< DcpuAsm.Stmt.iaq $a$ >>
            | "HWN"; a = asmoperand ->
              <:expr< DcpuAsm.Stmt.hwn $a$ >>
            | "HWQ"; a = asmoperand ->
              <:expr< DcpuAsm.Stmt.hwq $a$ >>
            | "HWI"; a = asmoperand ->
              <:expr< DcpuAsm.Stmt.hwi $a$ >>

            (* various helpers *)
            | "PASS" -> <:expr< DcpuAsm.Stmt.block [] >>
            | "ORG"; origin = a_INT ->
              let origin0 = int_of_string origin in
              if origin0 < 0 || origin0 >= 0x10000 then
                  raise (Stream.Error (Printf.sprintf "Invalid origin %#x"
                                                      origin0));
              <:expr< DcpuAsm.Stmt.org $int:origin$ >>
            | "ALIGN"; alignment = a_INT ->
              let alignment0 = int_of_string alignment in
              if alignment0 < 1 || alignment0 >= 0x10000 then
                  raise (Stream.Error (Printf.sprintf "Invalid alignment %#x"
                                                      alignment0));
              <:expr< DcpuAsm.Stmt.align $int:alignment$ >>
            | "NOP" -> <:expr< DcpuAsm.Stmt.nop >>
            | "JMP"; a = asmoperand -> <:expr< DcpuAsm.Stmt.jmp $a$ >>
            | "PUSH"; a = asmoperand -> <:expr< DcpuAsm.Stmt.push $a$ >>
            | "POP"; a = asmoperand -> <:expr< DcpuAsm.Stmt.pop $a$ >>
            | "RET" -> <:expr< DcpuAsm.Stmt.ret >>
            | "BRK" -> <:expr< DcpuAsm.Stmt.brk >>
            | "HLT" -> <:expr< DcpuAsm.Stmt.hlt >>
            ]
        ];

        asmlabeled:
        [
            [ l = asmlabelstr; ":"; e = expr LEVEL "top" ->
              <:expr< DcpuAsm.Stmt.label $l$ $e$ >>
            | l = asmlabelstr; ":" ->
              <:expr< DcpuAsm.Stmt.label $l$ (DcpuAsm.Stmt.block []) >>
            ]
        ];

        expr: LEVEL "top"
        [
            [ ls = TRY asmlabeled -> <:expr< $ls$ >> ]
        ];

        expr: BEFORE "simple"
        [
            "asmstmt" NONA
            [ s = asmstmt -> <:expr< $s$ >>
            | ls = TRY [
                l = asmlabelstr; s = asmstmt ->
                <:expr< DcpuAsm.Stmt.label $l$ $s$ >>
              ] -> <:expr< $ls$ >>
            | "BLOCK"; "LOCAL"; locals = asmlabelstrlist; ss = SELF ->
              <:expr< DcpuAsm.local $locals$ $ss$ >>
            | "BLOCK"; "LOCAL"; "#"; locals = expr LEVEL "simple"; OPT ","; ss = SELF ->
              <:expr< DcpuAsm.local $locals$ $ss$ >>
            | "BLOCK"; "LOCAL"; ss = SELF ->
              <:expr< DcpuAsm.local $list_of_locally_defined_labels _loc ss$
                                    $ss$ >>
            | "BLOCK"; ss = SELF ->
              <:expr< DcpuAsm.Stmt.block $ss$ >>
            ]
        ];

        expr: LEVEL "simple"
        [
            [ e = asmexpr LEVEL "simple" -> <:expr< $e$ >>
            | "ASM" -> <:expr< DcpuAsm.asm >>
            ]
        ];
    END
end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
