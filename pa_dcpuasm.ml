(* dcputhings: Assorted Tools for DCPU-16 Assembly.
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
            | <:expr< DcpuAsm.Asm__.label $str:label$ >> ->
              Hashtbl.replace defined label (); self
            | <:expr< DcpuAsm.AsmExpr__.label $str:label$ >> ->
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
        GLOBAL: a_LIDENT a_STRING expr;

        asmbarelabel:
        [
            [ label = a_LIDENT ->
              <:expr< DcpuAsm.AsmExpr__.label $str:label$ >>
            | "#"; e = expr LEVEL "simple" ->
              <:expr< DcpuAsm.AsmExpr__.label $e$ >>
            ]
        ];

        asmlabel:
        [
            [ "%"; l = asmbarelabel -> <:expr< $l$ >> ]
        ];

        asmlabelstr:
        [
            [ "%"; l = a_LIDENT -> <:expr< $str:l$ >>
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
            [ "A" -> <:expr< DcpuAsm.AsmExpr__.reg DcpuAsm.A >>
            | "B" -> <:expr< DcpuAsm.AsmExpr__.reg DcpuAsm.B >>
            | "C" -> <:expr< DcpuAsm.AsmExpr__.reg DcpuAsm.C >>
            | "X" -> <:expr< DcpuAsm.AsmExpr__.reg DcpuAsm.X >>
            | "Y" -> <:expr< DcpuAsm.AsmExpr__.reg DcpuAsm.Y >>
            | "Z" -> <:expr< DcpuAsm.AsmExpr__.reg DcpuAsm.Z >>
            | "I" -> <:expr< DcpuAsm.AsmExpr__.reg DcpuAsm.I >>
            | "J" -> <:expr< DcpuAsm.AsmExpr__.reg DcpuAsm.J >>
            ]
        ];

        asmexpr0: (* same as asmexpr but assumes that it starts with % *)
        [
            "+" LEFTA
            [ e1 = SELF; "+"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.add $e1$ $e2$ >>
            | e1 = SELF; "+%"; e2 = asmexpr0 LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.add $e1$ $e2$ >>
            | e1 = SELF; "-"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.sub $e1$ $e2$ >>
            | e1 = SELF; "-%"; e2 = asmexpr0 LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.sub $e1$ $e2$ >>
            | e1 = SELF; "OR"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.or_ $e1$ $e2$ >>
            | e1 = SELF; "XOR"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.xor $e1$ $e2$ >>
            ]
        |
            "*" LEFTA
            [ e1 = SELF; "*"; e2 = asmexpr LEVEL "~-" ->
              <:expr< DcpuAsm.AsmExpr__.mul $e1$ $e2$ >>
            | e1 = SELF; "*%"; e2 = asmexpr0 LEVEL "~-" ->
              <:expr< DcpuAsm.AsmExpr__.mul $e1$ $e2$ >>
            | e1 = SELF; "DIV"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.AsmExpr__.div $e1$ $e2$ >>
            | e1 = SELF; "MOD"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.AsmExpr__.mod_ $e1$ $e2$ >>
            | e1 = SELF; "AND"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.AsmExpr__.and_ $e1$ $e2$ >>
            ]
        |
            "shift" LEFTA
            [ e1 = SELF; "SHL"; e2 = asmexpr LEVEL "~-" ->
              <:expr< DcpuAsm.AsmExpr__.shl $e1$ $e2$ >>
            | e1 = SELF; "SHR"; e2 = asmexpr LEVEL "~-" ->
              <:expr< DcpuAsm.AsmExpr__.shr $e1$ $e2$ >>
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
            "+" LEFTA
            [ e1 = SELF; "+"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.add $e1$ $e2$ >>
            | e1 = SELF; "+%"; e2 = asmexpr0 LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.add $e1$ $e2$ >>
            | e1 = SELF; "-"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.sub $e1$ $e2$ >>
            | e1 = SELF; "-%"; e2 = asmexpr0 LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.sub $e1$ $e2$ >>
            | e1 = SELF; "OR"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.or_ $e1$ $e2$ >>
            | e1 = SELF; "XOR"; e2 = asmexpr LEVEL "*" ->
              <:expr< DcpuAsm.AsmExpr__.xor $e1$ $e2$ >>
            ]
        |
            "*" LEFTA
            [ e1 = SELF; "*"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.AsmExpr__.mul $e1$ $e2$ >>
            | e1 = SELF; "*%"; e2 = asmexpr0 LEVEL "shift" ->
              <:expr< DcpuAsm.AsmExpr__.mul $e1$ $e2$ >>
            | e1 = SELF; "DIV"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.AsmExpr__.div $e1$ $e2$ >>
            | e1 = SELF; "MOD"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.AsmExpr__.mod_ $e1$ $e2$ >>
            | e1 = SELF; "AND"; e2 = asmexpr LEVEL "shift" ->
              <:expr< DcpuAsm.AsmExpr__.and_ $e1$ $e2$ >>
            ]
        |
            "shift" LEFTA
            [ e1 = SELF; "SHL"; e2 = asmexpr LEVEL "~-" ->
              <:expr< DcpuAsm.AsmExpr__.shl $e1$ $e2$ >>
            | e1 = SELF; "SHR"; e2 = asmexpr LEVEL "~-" ->
              <:expr< DcpuAsm.AsmExpr__.shr $e1$ $e2$ >>
            ]
        |
            "~-" NONA
            [ "-"; e = asmexpr LEVEL "asm simple" ->
              <:expr< DcpuAsm.AsmExpr__.neg $e$ >>
            | "-%"; e = asmexpr0 LEVEL "asm simple" ->
              <:expr< DcpuAsm.AsmExpr__.neg $e$ >>
            | "NOT"; e = asmexpr LEVEL "asm simple" ->
              <:expr< DcpuAsm.AsmExpr__.not_ $e$ >>
            ]
        |
            "asm simple"
            [ v = a_INT ->
              <:expr< DcpuAsm.AsmExpr__.imm $int:v$ >>
            | v = a_CHAR ->
              <:expr< DcpuAsm.AsmExpr__.imm (int_of_char $chr:v$) >>
            | "["; e = asmexpr LEVEL "+"; "]" ->
              <:expr< DcpuAsm.AsmExpr__.mem $e$ >>
            | "[%"; e = asmexpr0 LEVEL "+"; "]" ->
              <:expr< DcpuAsm.AsmExpr__.mem $e$ >>
            | "("; e = asmexpr LEVEL "+"; ")" -> <:expr< $e$ >>
            | "(%"; e = asmexpr0 LEVEL "+"; ")" -> <:expr< $e$ >>
            | "#"; e = expr LEVEL "simple" -> <:expr< $e$ >>
            | i = val_longident -> <:expr< DcpuAsm.AsmExpr__.imm $id:i$ >>
            ]
        |
            "simple" NONA
            [ l = asmlabel -> <:expr< $l$ >> (* CAUTION! *)
            | r = asmreg -> <:expr< $r$ >>
            | "PC" -> <:expr< DcpuAsm.AsmExpr__.reg DcpuAsm.PC >>
            | "SP" -> <:expr< DcpuAsm.AsmExpr__.reg DcpuAsm.SP >>
            | "O" -> <:expr< DcpuAsm.AsmExpr__.reg DcpuAsm.O >>
            | "PUSH" -> <:expr< DcpuAsm.AsmExpr__.push >>
            | "PEEK" -> <:expr< DcpuAsm.AsmExpr__.peek >>
            | "POP" -> <:expr< DcpuAsm.AsmExpr__.pop >>
            | "VAL"; e = expr LEVEL "simple" ->
              <:expr< DcpuAsm.AsmExpr__.imm $e$ >>
            | "STR"; e = expr LEVEL "simple" ->
              <:expr< DcpuAsm.AsmExpr__.str $e$ >>
            | "PTR"; "["; e = asmexpr LEVEL "+"; "]" ->
              <:expr< DcpuAsm.AsmExpr__.mem $e$ >>
            | "PTR"; "[%"; e = asmexpr0 LEVEL "+"; "]" ->
              <:expr< DcpuAsm.AsmExpr__.mem $e$ >>
            | "IMM"; "("; e = asmexpr LEVEL "+"; ")" -> <:expr< $e$ >>
            | "IMM"; "(%"; e = asmexpr0 LEVEL "+"; ")" -> <:expr< $e$ >>
            | "LONG"; e = asmexpr -> <:expr< DcpuAsm.AsmExpr__.long $e$ >>
            ]
        ];

        asmoperand:
        [
            [ e = asmexpr -> <:expr< $e$ >>
            | e = expr LEVEL "simple" -> <:expr< $e$ >>
            ]
        ];

        asmdata:
        [
            [ ","; s = a_STRING; tail = asmdata ->
              <:expr< DcpuAsm.AsmExpr__.str $str:s$ :: $tail$ >>
            | ","; e = asmoperand; tail = asmdata ->
              <:expr< $e$ :: $tail$ >>
            | OPT "," -> <:expr< [] >>
            ]
        ];

        asmstmt:
        [
            [ "DAT"; s = a_STRING; tail = asmdata ->
              <:expr< DcpuAsm.Asm__.dat (DcpuAsm.AsmExpr__.str $str:s$ ::
                                         $tail$) >>
            | "DAT"; e = asmoperand; tail = asmdata ->
              <:expr< DcpuAsm.Asm__.dat ($e$ :: $tail$) >>
            | "SET"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.set $a$ $b$ >>
            | "ADD"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.add $a$ $b$ >>
            | "SUB"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.sub $a$ $b$ >>
            | "MUL"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.mul $a$ $b$ >>
            | "DIV"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.div $a$ $b$ >>
            | "MOD"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.mod_ $a$ $b$ >>
            | "SHL"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.shl $a$ $b$ >>
            | "SHR"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.shr $a$ $b$ >>
            | "AND"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.and_ $a$ $b$ >>
            | "BOR"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.bor $a$ $b$ >>
            | "XOR"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.xor $a$ $b$ >>
            | "IFE"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.ife $a$ $b$ >>
            | "IFN"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.ifn $a$ $b$ >>
            | "IFG"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.ifg $a$ $b$ >>
            | "IFB"; a = asmoperand; ","; b = asmoperand ->
              <:expr< DcpuAsm.Asm__.ifb $a$ $b$ >>
            | "JSR"; a = asmoperand ->
              <:expr< DcpuAsm.Asm__.jsr $a$ >>

            (* various helpers *)
            | "PASS" ->
              <:expr< DcpuAsm.Asm__.block [] >>
            | "JMP"; a = asmoperand ->
              <:expr< DcpuAsm.Asm__.set (DcpuAsm.AsmExpr__.reg DcpuAsm.PC)
                                        $a$ >>
            | "PUSH"; a = asmoperand ->
              <:expr< DcpuAsm.Asm__.set DcpuAsm.AsmExpr__.push $a$ >>
            | "POP"; a = asmoperand ->
              <:expr< DcpuAsm.Asm__.set $a$ DcpuAsm.AsmExpr__.pop >>
            | "BRK" ->
              <:expr< DcpuAsm.Asm__.sub (DcpuAsm.AsmExpr__.reg DcpuAsm.PC)
                                        (DcpuAsm.AsmExpr__.imm 1) >>
            | "HLT" ->
              <:expr< DcpuAsm.Asm__.sub (DcpuAsm.AsmExpr__.reg DcpuAsm.PC)
                                        (DcpuAsm.AsmExpr__.imm 1) >>
            ]
        ];

        asmlabeled:
        [
            [ l = asmlabelstr; ":"; e = expr LEVEL "top" ->
              <:expr< DcpuAsm.Asm__.label $l$ $e$ >>
            | l = asmlabelstr; ":" ->
              <:expr< DcpuAsm.Asm__.label $l$ (DcpuAsm.Asm__.block []) >>
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
                <:expr< DcpuAsm.Asm__.label $l$ $s$ >>
              ] -> <:expr< $ls$ >>
            | "BLOCK"; "LOCAL"; locals = asmlabelstrlist; ss = SELF ->
              <:expr< DcpuAsm.local $locals$ $ss$ >>
            | "BLOCK"; "LOCAL"; "#"; locals = expr LEVEL "simple"; OPT ","; ss = SELF ->
              <:expr< DcpuAsm.local $locals$ $ss$ >>
            | "BLOCK"; "LOCAL"; ss = SELF ->
              <:expr< DcpuAsm.local $list_of_locally_defined_labels _loc ss$
                                    $ss$ >>
            | "BLOCK"; ss = SELF ->
              <:expr< DcpuAsm.Asm__.block $ss$ >>
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
