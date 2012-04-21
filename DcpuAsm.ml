(* dcputhings: Assorted Tools for DCPU-16 Development.
 * Written by Kang Seonghoon. See LICENSE for the full license statement.
 *)

let repeat n l =
    let rec recur n revl acc =
        if n > 0 then
            recur (n-1) revl (List.rev_append revl acc)
        else
            acc
    in recur n (List.rev l) []

(**********************************************************************)
(* Assembly AST. *)

type reg = A | B | C | X | Y | Z | I | J | PC | SP | O
type label = string

let string_of_reg = function
    | A -> "A" | B -> "B" | C -> "C" | X -> "X" | Y -> "Y" | Z -> "Z"
    | I -> "I" | J -> "J" | PC -> "PC" | SP -> "SP" | O -> "O"

let index_of_reg = function
    | A -> 0 | B -> 1 | C -> 2 | X -> 3 | Y -> 4 | Z -> 5 | I -> 6 | J -> 7
    | PC | SP | O -> failwith "index_of_reg"

let gensym prefix =
    let counter = ref (-1) in
    let prefix = prefix ^ "(" in
    function () -> incr counter; prefix ^ string_of_int !counter ^ ")"

(* an expression which will be eventually resolved to a single immediate. *)
type expr =
    | ENum of int
    | ELabel of label
    | ENeg of expr
    | ENot of expr
    | EAdd of expr * expr
    | ESub of expr * expr
    | EMul of expr * expr
    | EDiv of expr * expr
    | EMod of expr * expr
    | EAnd of expr * expr
    | EOr  of expr * expr
    | EXor of expr * expr
    | EShl of expr * expr
    | EShr of expr * expr

let string_of_expr e =
    let rec show prec e =
        let paren prec' s = if prec > prec' then "(" ^ s ^ ")" else s in
        match e with
        | ENum v       -> string_of_int v
        | ELabel l     -> "%" ^ l
        | ENeg e       -> paren 3 (             "-"    ^ show 3 e )
        | ENot e       -> paren 3 (             "NOT " ^ show 3 e )
        | EShl (e1,e2) -> paren 2 (show 3 e1 ^ " SHL " ^ show 2 e2)
        | EShr (e1,e2) -> paren 2 (show 3 e1 ^ " SHR " ^ show 2 e2)
        | EMul (e1,e2) -> paren 1 (show 2 e1 ^  "*"    ^ show 1 e2)
        | EDiv (e1,e2) -> paren 1 (show 2 e1 ^ " DIV " ^ show 1 e2)
        | EMod (e1,e2) -> paren 1 (show 2 e1 ^ " MOD " ^ show 1 e2)
        | EAnd (e1,e2) -> paren 1 (show 2 e1 ^ " AND " ^ show 1 e2)
        | EAdd (e1,e2) -> paren 0 (show 1 e1 ^  "+"    ^ show 0 e2)
        | ESub (e1,e2) -> paren 0 (show 1 e1 ^  "-"    ^ show 0 e2)
        | EOr  (e1,e2) -> paren 0 (show 1 e1 ^ " OR "  ^ show 0 e2)
        | EXor (e1,e2) -> paren 0 (show 1 e1 ^ " XOR " ^ show 0 e2)
    in show 0 e

let rec eval_expr resolve e =
    let recur = eval_expr resolve in
    match e with
    | ENum v -> ENum v
    | ELabel l -> resolve l
    | ENeg e ->
        begin match recur e with
        | ENum v -> ENum (-v)
        | e -> e
        end
    | ENot e ->
        begin match recur e with
        | ENum v -> ENum (lnot v)
        | e -> e
        end
    | EAdd (e1,e2) ->
        begin match recur e1, recur e2 with
        | ENum v1, ENum v2 -> ENum (v1+v2)
        | e1, e2 -> EAdd (e1, e2)
        end
    | ESub (e1,e2) ->
        begin match recur e1, recur e2 with
        | ENum v1, ENum v2 -> ENum (v1-v2)
        | e1, e2 -> ESub (e1, e2)
        end
    | EMul (e1,e2) ->
        begin match recur e1, recur e2 with
        | ENum v1, ENum v2 -> ENum (v1*v2)
        | e1, e2 -> EMul (e1, e2)
        end
    | EDiv (e1,e2) ->
        begin match recur e1, recur e2 with
        | ENum v1, ENum v2 -> ENum (v1/v2)
        | e1, e2 -> EDiv (e1, e2)
        end
    | EMod (e1,e2) ->
        begin match recur e1, recur e2 with
        | ENum v1, ENum v2 -> ENum (v1 mod v2)
        | e1, e2 -> EMod (e1, e2)
        end
    | EAnd (e1,e2) ->
        begin match recur e1, recur e2 with
        | ENum v1, ENum v2 -> ENum (v1 land v2)
        | e1, e2 -> EAnd (e1, e2)
        end
    | EOr (e1,e2) ->
        begin match recur e1, recur e2 with
        | ENum v1, ENum v2 -> ENum (v1 lor v2)
        | e1, e2 -> EOr (e1, e2)
        end
    | EXor (e1,e2) ->
        begin match recur e1, recur e2 with
        | ENum v1, ENum v2 -> ENum (v1 lxor v2)
        | e1, e2 -> EXor (e1, e2)
        end
    | EShl (e1,e2) ->
        begin match recur e1, recur e2 with
        | ENum v1, ENum v2 -> ENum (v1 lsl v2)
        | e1, e2 -> EShl (e1, e2)
        end
    | EShr (e1,e2) ->
        begin match recur e1, recur e2 with
        | ENum v1, ENum v2 -> ENum (v1 lsr v2)
        | e1, e2 -> EShr (e1, e2)
        end

(* DCPU-16 operand. (termed "value" in the official spec)
 * exprs below should be "ENum v" in order to compile. *)
type value =
    | Reg of reg
    | MemReg of reg
    | MemRegLit of reg * expr
    | Pop
    | Peek
    | Push
    | MemNext
    | Next
    | MemLit of expr
    | Lit of expr
    | LitShort of expr
    | LitLong of expr

let string_of_value = function
    | Reg r -> string_of_reg r
    | MemReg r -> "[" ^ string_of_reg r ^ "]"
    | MemRegLit (r, ENum v) ->
        let v = v land 0xffff in
        if v < 0x8000 then
            "[" ^ string_of_reg r ^ "+" ^ string_of_int v ^ "]"
        else
            "[" ^ string_of_reg r ^ "-" ^ string_of_int (-v) ^ "]"
    | MemRegLit (r, e) ->
        "[" ^ string_of_reg r ^ "+" ^ string_of_expr e ^ "]"
    | Pop -> "POP"
    | Peek -> "PEEK"
    | Push -> "PUSH"
    | MemNext -> "[<next word>]"
    | Next -> "<next word>"
    | MemLit (ENum v) -> "[" ^ string_of_int (v land 0xffff) ^ "]"
    | MemLit e -> "[" ^ string_of_expr e ^ "]"
    | Lit (ENum v) -> string_of_int (v land 0xffff)
    | Lit e -> string_of_expr e
    | LitShort (ENum v) -> "SHORT " ^ string_of_int (v land 0xffff)
    | LitShort e -> "SHORT " ^ string_of_expr e
    | LitLong (ENum v) -> "LONG " ^ string_of_int (v land 0xffff)
    | LitLong e -> "LONG " ^ string_of_expr e

let force_shorter_value = function
    | Lit e -> LitShort e
    | v -> v

let force_longer_value = function
    | Lit e -> LitLong e
    | v -> v

let eval_value resolve = function
    | MemRegLit (r,e) ->
        let e = eval_expr resolve e in
        if e = ENum 0 then MemReg r else MemRegLit (r,e)
    | MemLit e -> MemLit (eval_expr resolve e)
    | Lit e -> Lit (eval_expr resolve e)
    | LitShort e -> LitShort (eval_expr resolve e)
    | LitLong e -> LitLong (eval_expr resolve e)
    | v -> v

(* DCPU-16 instructions. *)
type instr =
    | Dat of expr * expr        (* DAT [expr TIMES] expr (pseudo) *)
    | DatStr of expr * int list (* DAT [expr TIMES] "string" (pseudo) *)
    | Res of expr               (* DAT [expr TIMES] _ (pseudo) *)
    | Set of value * value      (* SET lhs, rhs *)
    | Add of value * value      (* ADD lhs, rhs *)
    | Sub of value * value      (* SUB lhs, rhs *)
    | Mul of value * value      (* MUL lhs, rhs *)
    | Div of value * value      (* DIV lhs, rhs *)
    | Mod of value * value      (* MOD lhs, rhs *)
    | Shl of value * value      (* SHL lhs, rhs *)
    | Shr of value * value      (* SHR lhs, rhs *)
    | And of value * value      (* AND lhs, rhs *)
    | Bor of value * value      (* BOR lhs, rhs *)
    | Xor of value * value      (* XOR lhs, rhs *)
    | Ife of value * value      (* IFE lhs, rhs *)
    | Ifn of value * value      (* IFN lhs, rhs *)
    | Ifg of value * value      (* IFG lhs, rhs *)
    | Ifb of value * value      (* IFB lhs, rhs *)
    | Jsr of value              (* JSR lhs *)
    | Jmp of value              (* JMP lhs (pseudo) *)

let string_of_instr =
    let times = function | ENum 1 -> ""
                         | c -> string_of_expr c ^ " TIMES " in
    function
    | Dat (c,e) -> "DAT " ^ times c ^ string_of_expr e
    | DatStr (c,s) ->
        let data = List.map (fun v -> Printf.sprintf "%x" v) s in
        "DAT " ^ times c ^ "(" ^ (String.concat ", " data) ^ ")"
    | Res c -> "DAT " ^ times c ^ "_"
    | Set (a,b) -> "SET " ^ string_of_value a ^ ", " ^ string_of_value b
    | Add (a,b) -> "ADD " ^ string_of_value a ^ ", " ^ string_of_value b
    | Sub (a,b) -> "SUB " ^ string_of_value a ^ ", " ^ string_of_value b
    | Mul (a,b) -> "MUL " ^ string_of_value a ^ ", " ^ string_of_value b
    | Div (a,b) -> "DIV " ^ string_of_value a ^ ", " ^ string_of_value b
    | Mod (a,b) -> "MOD " ^ string_of_value a ^ ", " ^ string_of_value b
    | Shl (a,b) -> "SHL " ^ string_of_value a ^ ", " ^ string_of_value b
    | Shr (a,b) -> "SHR " ^ string_of_value a ^ ", " ^ string_of_value b
    | And (a,b) -> "AND " ^ string_of_value a ^ ", " ^ string_of_value b
    | Bor (a,b) -> "BOR " ^ string_of_value a ^ ", " ^ string_of_value b
    | Xor (a,b) -> "XOR " ^ string_of_value a ^ ", " ^ string_of_value b
    | Ife (a,b) -> "IFE " ^ string_of_value a ^ ", " ^ string_of_value b
    | Ifn (a,b) -> "IFN " ^ string_of_value a ^ ", " ^ string_of_value b
    | Ifg (a,b) -> "IFG " ^ string_of_value a ^ ", " ^ string_of_value b
    | Ifb (a,b) -> "IFB " ^ string_of_value a ^ ", " ^ string_of_value b
    | Jsr a -> "JSR " ^ string_of_value a
    | Jmp a -> "JMP " ^ string_of_value a

let force_instr force =
    function
    | Dat (c,e) -> Dat (c,e)
    | DatStr (c,s) -> DatStr (c,s)
    | Res c -> Res c
    | Set (a,b) -> Set (force a, force b)
    | Add (a,b) -> Add (force a, force b)
    | Sub (a,b) -> Sub (force a, force b)
    | Mul (a,b) -> Mul (force a, force b)
    | Div (a,b) -> Div (force a, force b)
    | Mod (a,b) -> Mod (force a, force b)
    | Shl (a,b) -> Shl (force a, force b)
    | Shr (a,b) -> Shr (force a, force b)
    | And (a,b) -> And (force a, force b)
    | Bor (a,b) -> Bor (force a, force b)
    | Xor (a,b) -> Xor (force a, force b)
    | Ife (a,b) -> Ife (force a, force b)
    | Ifn (a,b) -> Ifn (force a, force b)
    | Ifg (a,b) -> Ifg (force a, force b)
    | Ifb (a,b) -> Ifb (force a, force b)
    | Jsr a -> Jsr (force a)
    | Jmp a -> Jmp (force a)

let force_shorter_instr = force_instr force_shorter_value
let force_longer_instr = force_instr force_longer_value

let eval_instr resolve =
    let evale = eval_expr resolve in
    let evalv = eval_value resolve in
    function
    | Dat (c,e) -> Dat (evale c, evale e)
    | DatStr (c,s) -> DatStr (evale c, s)
    | Res c -> Res (evale c)
    | Set (a,b) -> Set (evalv a, evalv b)
    | Add (a,b) -> Add (evalv a, evalv b)
    | Sub (a,b) -> Sub (evalv a, evalv b)
    | Mul (a,b) -> Mul (evalv a, evalv b)
    | Div (a,b) -> Div (evalv a, evalv b)
    | Mod (a,b) -> Mod (evalv a, evalv b)
    | Shl (a,b) -> Shl (evalv a, evalv b)
    | Shr (a,b) -> Shr (evalv a, evalv b)
    | And (a,b) -> And (evalv a, evalv b)
    | Bor (a,b) -> Bor (evalv a, evalv b)
    | Xor (a,b) -> Xor (evalv a, evalv b)
    | Ife (a,b) -> Ife (evalv a, evalv b)
    | Ifn (a,b) -> Ifn (evalv a, evalv b)
    | Ifg (a,b) -> Ifg (evalv a, evalv b)
    | Ifb (a,b) -> Ifb (evalv a, evalv b)
    | Jsr a -> Jsr (evalv a)
    | Jmp a -> Jmp (evalv a)

let eval_instr_with_pc cur resolve =
    let eval = eval_expr resolve in

    let is_short_literal = function
        | ENum v -> (v land 0xffff < 32)
        | _ -> false in

    (* we only convert operands which are short constants (so they cannot
     * cause SHORT to err), but for the safety. *)
    let convert_to_short = function
        | ENum v -> LitShort (ENum (v land 0xffff))
        | _ -> failwith "impossible" in

    function
    | Jmp (Lit e) ->
        (* check four possible operands for comparison.
         * note that opcodes other than SET need to use only one word,
         * so the PC during the evalution of (Lit e) is always (cur + 1). *)
        let aminuspc = eval (ESub (e, ENum (cur + 1))) in
        let pcminusa = eval (ESub (ENum (cur + 1), e)) in
        let axorpc = eval (EXor (e, ENum (cur + 1))) in
        let a = eval e in
        if is_short_literal axorpc then
            (* use "XOR PC, ...". the most efficient (1 cycle), useful for
             * short relative jumps. *)
            Xor (Reg PC, convert_to_short axorpc)
        else if is_short_literal a then
            (* use "SET PC, ...". if the operand is a short literal then it
             * takes one cycle, but is only applicable for very few labels. *)
            Set (Reg PC, convert_to_short a)
        else if is_short_literal aminuspc then
            (* use "ADD PC, ...". it takes as same number of cycles as
             * SET with e long literal but one word shorter. *)
            Add (Reg PC, convert_to_short aminuspc)
        else if is_short_literal pcminusa then
            (* use "SUB PC, ...". same as "ADD PC, ...". *)
            Sub (Reg PC, convert_to_short pcminusa)
        else
            (* otherwise "SET PC, ..." takes 2 cycles and optimal. *)
            Set (Reg PC, Lit a)

    | Jmp a ->
        (* if the operand is not a literal or explicit SHORT or LONG encoding
         * is requested then we fall back to the usual encoding. *)
        Set (Reg PC, eval_value resolve a)

    | ins -> eval_instr resolve ins

(**********************************************************************)
(* Assembly Expressions. (e.g. [A+0x42]) *)

type asmexpr =
    | AsmImm of int                 (* 42 *)
    | AsmShort of asmexpr           (* SHORT expr *)
    | AsmLong of asmexpr            (* LONG expr *)
    | AsmStr of string              (* "string" *)
    | AsmReg of reg                 (* A, B, C, X, Y, Z, I, J, PC, SP, O *)
    | AsmPush                       (* [--SP] or PUSH *)
    | AsmPeek                       (* [SP] or PEEK *)
    | AsmPop                        (* [SP++] or POP *)
    | AsmLabel of label             (* %label *)
    | AsmBlank                      (* _ *)
    | AsmTimes of asmexpr * asmexpr (* expr TIMES expr *)
    | AsmMem of asmexpr             (* [expr] *)
    | AsmNeg of asmexpr             (* -expr *)
    | AsmNot of asmexpr             (* NOT expr *)
    | AsmAdd of asmexpr * asmexpr   (* lhs + rhs *)
    | AsmSub of asmexpr * asmexpr   (* lhs - rhs *)
    | AsmMul of asmexpr * asmexpr   (* lhs * rhs *)
    | AsmDiv of asmexpr * asmexpr   (* lhs DIV rhs *)
    | AsmMod of asmexpr * asmexpr   (* lhs MOD rhs *)
    | AsmAnd of asmexpr * asmexpr   (* lhs AND rhs *)
    | AsmOr  of asmexpr * asmexpr   (* lhs OR rhs *)
    | AsmXor of asmexpr * asmexpr   (* lhs XOR rhs *)
    | AsmShl of asmexpr * asmexpr   (* lhs SHL rhs *)
    | AsmShr of asmexpr * asmexpr   (* lhs SHR rhs *)

module AsmExpr__ = struct
    let imm v      = AsmImm v
    let short e    = AsmShort e
    let long e     = AsmLong e
    let str s      = AsmStr s
    let reg r      = AsmReg r
    let push       = AsmPush
    let peek       = AsmPeek
    let pop        = AsmPop
    let label l    = AsmLabel l
    let blank      = AsmBlank
    let times c e  = AsmTimes (c,e)
    let mem  e     = AsmMem e
    let neg  e     = AsmNeg e
    let not_ e     = AsmNot e
    let add  e1 e2 = AsmAdd (e1,e2)
    let sub  e1 e2 = AsmSub (e1,e2)
    let mul  e1 e2 = AsmMul (e1,e2)
    let div  e1 e2 = AsmDiv (e1,e2)
    let mod_ e1 e2 = AsmMod (e1,e2)
    let and_ e1 e2 = AsmAnd (e1,e2)
    let or_  e1 e2 = AsmOr  (e1,e2)
    let xor  e1 e2 = AsmXor (e1,e2)
    let shl  e1 e2 = AsmShl (e1,e2)
    let shr  e1 e2 = AsmShr (e1,e2)
end

let gen_selflabel = gensym "<self>"

let parse_asmexpr e =
    let rescale k l =
        if k = 0 then [] else List.rev_map (fun (r,c) -> (r,c*k)) l in

    let combine l1 l2 =
        if l1 = [] then
            l2
        else if l2 = [] then
            l1
        else
            let l = l1 @ l2 in
            let gather r =
                let filtered = List.filter (fun (r',c) -> r = r') l in
                let sum = List.fold_left (+) 0 (List.rev_map snd filtered) in
                if sum = 0 then [] else [(r,sum)]
            in List.concat (List.rev_map gather [A;B;C;X;Y;Z;I;J;PC;SP;O])
    in

    let selflabel = ref None in

    (* canonicalizes asmexpr: linear combination of registers plus
     * an immediate which may contain labels to be resolved later. *)
    let rec canonicalize = function
        | AsmImm v -> ([], ENum v, false)
        | AsmShort e -> canonicalize e
        | AsmLong e -> canonicalize e
        | AsmStr _ -> failwith "parse_asmexpr: unexpected string"
        | AsmReg r -> ([(r,1)], ENum 0, false)
        | AsmLabel "_" -> (* pseudolabel *)
            let l =
                match !selflabel with
                | None -> let l' = gen_selflabel () in selflabel := Some l'; l'
                | Some l' -> l'
            in ([], ELabel l, true)
        | AsmLabel l -> ([], ELabel l, false)
        | AsmBlank -> failwith "parse_asmexpr: _ can only be used in DAT"
        | AsmTimes _ -> failwith "parse_asmexpr: TIMES can only be used in DAT"
        | AsmMem e -> failwith "parse_asmexpr: nested memory reference"
        | AsmPop -> failwith "parse_asmexpr: unexpected POP"
        | AsmPeek -> failwith "parse_asmexpr: unexpected PEEK"
        | AsmPush -> failwith "parse_asmexpr: unexpected PUSH"

        (* a basic bit of constant propagation. we won't do the AST rotation
         * here, just make sure that we properly handle the linear combination
         * of registers (so that [A*2-A+3] is permitted, for example). *)

        | AsmNeg e ->
            let (regs, e, hasself) = canonicalize e in
            let regs = rescale (-1) regs in
            begin match e with
            | ENum v -> (regs, ENum (-v), false)
            | e      -> (regs, ENeg e, hasself)
            end

        | AsmNot e ->
            let (regs, e, hasself) = canonicalize e in
            if regs = [] then
                begin match e with
                | ENum v -> ([], ENum (lnot v), false)
                | e      -> ([], ENot e, hasself)
                end
            else
                failwith "parse_asmexpr: non-linear register"

        | AsmAdd (e1,e2) ->
            let (regs1, e1, hasself1) = canonicalize e1 in
            let (regs2, e2, hasself2) = canonicalize e2 in
            let regs = combine regs1 regs2 in
            begin match e1, e2 with
            | ENum v1, ENum v2 -> (regs, ENum (v1+v2), false)
            | ENum 0,  e2      -> (regs, e2, hasself2)
            | e1,      ENum 0  -> (regs, e1, hasself1)
            | e1,      e2      -> (regs, EAdd (e1, e2), hasself1 || hasself2)
            end

        | AsmSub (e1,e2) ->
            let (regs1, e1, hasself1) = canonicalize e1 in
            let (regs2, e2, hasself2) = canonicalize e2 in
            let regs = combine regs1 (rescale (-1) regs2) in
            begin match e1, e2 with
            | ENum v1, ENum v2 -> (regs, ENum (v1-v2), false)
            | ENum 0,  e2      -> (regs, ENeg e2, hasself2)
            | e1,      ENum 0  -> (regs, e1, hasself1)
            | e1,      e2      -> (regs, ESub (e1, e2), hasself1 || hasself2)
            end

        | AsmMul (e1,e2) ->
            let (regs1, e1, hasself1) = canonicalize e1 in
            let (regs2, e2, hasself2) = canonicalize e2 in
            begin match e1, e2 with
            | ENum v1, ENum v2 ->
                let regs = match regs1, regs2 with
                    | [], [] -> []
                    | _,  [] -> rescale v2 regs1
                    | [], _  -> rescale v1 regs2
                    | _,  _  -> failwith "parse_asmexpr: non-linear register"
                in (regs, ENum (v1*v2), false)
            | ENum v1, e2 ->
                let regs = match regs1, regs2 with
                    | [], [] -> []
                    | [], _  -> rescale v1 regs2
                    | _,  _  -> failwith "parse_asmexpr: non-linear register"
                in (regs, EMul (e1, e2), hasself1 || hasself2)
            | e1, ENum v2 ->
                let regs = match regs1, regs2 with
                    | [], [] -> []
                    | _,  [] -> rescale v2 regs1
                    | _,  _  -> failwith "parse_asmexpr: non-linear register"
                in (regs, EMul (e1, e2), hasself1 || hasself2)
            | e1, e2 ->
                if regs1 = [] && regs2 = [] then
                    ([], EMul (e1, e2), hasself1 || hasself2)
                else
                    failwith "parse_asmexpr: non-linear register"
            end

        | AsmDiv (e1,e2) ->
            let (regs1, e1, hasself1) = canonicalize e1 in
            let (regs2, e2, hasself2) = canonicalize e2 in
            if regs1 = [] && regs2 = [] then
                begin match e1, e2 with
                | ENum v1, ENum v2 -> ([], ENum (v1/v2), false)
                | e1,      e2      -> ([], EDiv (e1, e2), hasself1 || hasself2)
                end
            else
                failwith "parse_asmexpr: non-linear register"

        | AsmMod (e1,e2) ->
            let (regs1, e1, hasself1) = canonicalize e1 in
            let (regs2, e2, hasself2) = canonicalize e2 in
            if regs1 = [] && regs2 = [] then
                begin match e1, e2 with
                | ENum v1, ENum v2 -> ([], ENum (v1 mod v2), false)
                | e1,      e2      -> ([], EMod (e1, e2), hasself1 || hasself2)
                end
            else
                failwith "parse_asmexpr: non-linear register"

        | AsmAnd (e1,e2) ->
            let (regs1, e1, hasself1) = canonicalize e1 in
            let (regs2, e2, hasself2) = canonicalize e2 in
            if regs1 = [] && regs2 = [] then
                begin match e1, e2 with
                | ENum v1, ENum v2 -> ([], ENum (v1 land v2), false)
                | e1,      e2      -> ([], EAnd (e1, e2), hasself1 || hasself2)
                end
            else
                failwith "parse_asmexpr: non-linear register"

        | AsmOr (e1,e2) ->
            let (regs1, e1, hasself1) = canonicalize e1 in
            let (regs2, e2, hasself2) = canonicalize e2 in
            if regs1 = [] && regs2 = [] then
                begin match e1, e2 with
                | ENum v1, ENum v2 -> ([], ENum (v1 lor v2), false)
                | e1,      e2      -> ([], EOr (e1, e2), hasself1 || hasself2)
                end
            else
                failwith "parse_asmexpr: non-linear register"

        | AsmXor (e1,e2) ->
            let (regs1, e1, hasself1) = canonicalize e1 in
            let (regs2, e2, hasself2) = canonicalize e2 in
            if regs1 = [] && regs2 = [] then
                begin match e1, e2 with
                | ENum v1, ENum v2 -> ([], ENum (v1 lxor v2), false)
                | e1,      e2      -> ([], EXor (e1, e2), hasself1 || hasself2)
                end
            else
                failwith "parse_asmexpr: non-linear register"

        | AsmShl (e1,e2) ->
            let (regs1, e1, hasself1) = canonicalize e1 in
            let (regs2, e2, hasself2) = canonicalize e2 in
            if regs1 = [] && regs2 = [] then
                begin match e1, e2 with
                | ENum v1, ENum v2 -> ([], ENum (v1 lsl v2), false)
                | e1,      e2      -> ([], EShl (e1, e2), hasself1 || hasself2)
                end
            else
                failwith "parse_asmexpr: non-linear register"

        | AsmShr (e1,e2) ->
            let (regs1, e1, hasself1) = canonicalize e1 in
            let (regs2, e2, hasself2) = canonicalize e2 in
            if regs1 = [] && regs2 = [] then
                begin match e1, e2 with
                | ENum v1, ENum v2 -> ([], ENum (v1 lsr v2), false)
                | e1,      e2      -> ([], EShr (e1, e2), hasself1 || hasself2)
                end
            else
                failwith "parse_asmexpr: non-linear register"
    in

    match e with
    | AsmPush -> (Push, None)
    | AsmPeek -> (Peek, None)
    | AsmPop -> (Pop, None)
    | AsmMem e ->
        let (regs, e', hasself) = canonicalize e in
        let v =
            match regs, e' with
            | [], imm -> MemLit imm
            | [((A|B|C|X|Y|Z|I|J) as r, 1)], ENum 0 -> MemReg r
            | [((A|B|C|X|Y|Z|I|J) as r, 1)], imm -> MemRegLit (r,imm)
            | [(SP, 1)], ENum 0 -> Peek
            | [(r, 1)], imm ->
                failwith ("parse_asmexpr: unsupported register " ^
                          string_of_reg r ^ " in memory reference")
            | [(r, k)], imm ->
                failwith ("parse_asmexpr: non-linear register " ^
                          string_of_reg r)
            | _, _ ->
                failwith "parse_asmexpr: multiple registers in memory reference"
        in (v, if hasself then !selflabel else None)
    | e ->
        let (regs, e', hasself) = canonicalize e in
        let v =
            match regs, e' with
            | [], imm ->
                begin match e with
                | AsmShort _ -> LitShort imm
                | AsmLong _ -> LitLong imm
                | _ -> Lit imm
                end
            | [(r, 1)], ENum 0 ->
                Reg r
            | [(r, k)], imm ->
                failwith ("parse_asmexpr: unsupported arithmetic on register " ^
                          string_of_reg r)
            | _, _ ->
                failwith "parse_asmexpr: unsupported arithmetic on registers"
        in (v, if hasself then !selflabel else None)

(**********************************************************************)
(* Assembly Statements. (e.g. SUB SP, 1) *)

type stmt =
    | Nothing
    | Empty of int
    | Static of int list
    | Dynamic of instr * int * int    (* instr, min length, max length *)
    | Labeled of label * stmt
    | Blocked of stmt list

let print_stmt s =
    let rec show indent = function
        | Nothing -> ()
        | Empty v ->
            Printf.printf "%s  DAT %d TIMES _\n" indent v
        | Static cs ->
            let data = List.map (fun c -> Printf.sprintf "0x%04x" c) cs in
            Printf.printf "%s  DAT %s\n" indent (String.concat ", " data)
        | Dynamic (i,min,max) ->
            Printf.printf "%s  %s    ; min=%d max=%d\n"
                indent (string_of_instr i) min max
        | Labeled (l,s) ->
            Printf.printf "%s%%%s:\n" indent l;
            show indent s
        | Blocked is ->
            List.iter (show indent) is
    in show "" s

type compile_result =
    | Done of int list
    | DoneEmpty of int
    | NotYet of int * int       (* min length, max length *)

let compile_value = function
    | Reg SP -> Done [27]
    | Reg PC -> Done [28]
    | Reg O -> Done [29]
    | Reg r -> Done [index_of_reg r]
    | MemReg r -> Done [index_of_reg r + 8]
    | MemRegLit (r, ENum v) -> Done [index_of_reg r + 16; v land 0xffff]
    | MemRegLit (r, e) -> NotYet (1, 1)
    | Pop -> Done [24]
    | Peek -> Done [25]
    | Push -> Done [26]
    | MemNext -> Done [30]
    | Next -> Done [31]
    | MemLit (ENum v) -> Done [30; v land 0xffff]
    | MemLit e -> NotYet (1, 1)
    | Lit (ENum v) ->
        let v = v land 0xffff in
        if v < 32 then Done [32 + v] else Done [31; v]
    | Lit e -> NotYet (0, 1)
    | LitShort (ENum v) ->
        let v = v land 0xffff in
        if v < 32 then
            Done [32 + v]
        else
            failwith "compile_value: failed to satisfy SHORT constraint"
    | LitShort e -> NotYet (0, 0)
    | LitLong (ENum v) -> Done [31; v]
    | LitLong e -> NotYet (1, 1)

let compile_instr =
    let unary o a =
        match compile_value a with
        | Done [] -> failwith "impossible"
        | Done (a::anext) -> Done ((a lsl 10) lor (o lsl 4) :: anext)
        | DoneEmpty _ -> failwith "impossible"
        | NotYet (amin,amax) -> NotYet (1+amin, 1+amax)
    in

    let binary o a b =
        match compile_value a, compile_value b with
        | Done [], _ | _, Done [] -> failwith "impossible"
        | Done (a::anext), Done (b::bnext) ->
            Done ((b lsl 10) lor (a lsl 4) lor o :: anext @ bnext)
        | Done (_::anext), NotYet (bmin,bmax) ->
            let alen = List.length anext in
            NotYet (1+alen+bmin, 1+alen+bmax)
        | DoneEmpty _, _ | _, DoneEmpty _ -> failwith "impossible"
        | NotYet (amin,amax), Done (_::bnext) ->
            let blen = List.length bnext in
            NotYet (1+amin+blen, 1+amax+blen)
        | NotYet (amin,amax), NotYet (bmin,bmax) ->
            NotYet (1+amin+bmin, 1+amax+bmax)
    in

    let jump a =
        match compile_value a with
        | Done [] -> failwith "impossible"
        | Done (a::anext) -> Done ((a lsl 10) lor 0x01c1 :: anext)
        | DoneEmpty _ -> failwith "impossible"
        | NotYet (amin,amax) -> NotYet (max 1 amin, 1+amax)
    in

    function
    | Dat (ENum c, ENum v) ->
        if c < 0 then failwith "compile_instr: TIMES with a negative count";
        Done (repeat c [v land 0xffff])
    | Dat (ENum c, e) ->
        if c < 0 then failwith "compile_instr: TIMES with a negative count";
        NotYet (c, c)
    | Dat (ce, e) ->
        NotYet (0, 65535)
    | DatStr (ENum c, s) ->
        if c < 0 then failwith "compile_instr: TIMES with a negative count";
        Done (repeat c s)
    | DatStr (ce, s) ->
        NotYet (0, 65535)
    | Res (ENum v) ->
        if v < 0 then failwith "compile_instr: TIMES with a negative count";
        DoneEmpty v
    | Res c ->
        NotYet (0, 65535)

    | Set (a,b) -> binary  1 a b
    | Add (a,b) -> binary  2 a b
    | Sub (a,b) -> binary  3 a b
    | Mul (a,b) -> binary  4 a b
    | Div (a,b) -> binary  5 a b
    | Mod (a,b) -> binary  6 a b
    | Shl (a,b) -> binary  7 a b
    | Shr (a,b) -> binary  8 a b
    | And (a,b) -> binary  9 a b
    | Bor (a,b) -> binary 10 a b
    | Xor (a,b) -> binary 11 a b
    | Ife (a,b) -> binary 12 a b
    | Ifn (a,b) -> binary 13 a b
    | Ifg (a,b) -> binary 14 a b
    | Ifb (a,b) -> binary 15 a b
    | Jsr a     -> unary   1 a
    | Jmp a     -> jump      a

let attach_label l0 s =
    match l0 with
    | Some l -> Labeled (l,s)
    | None -> s

let make_instr ins =
    match compile_instr ins with
    | Done cs -> Static cs
    | DoneEmpty v -> Empty v
    | NotYet (min,max) -> Dynamic (ins,min,max)

let make_unary_instr f a =
    let (a,aself) = parse_asmexpr a in
    let s = make_instr (f a) in
    attach_label aself s

let make_binary_instr f a b =
    let (a,aself) = parse_asmexpr a in
    let (b,bself) = parse_asmexpr b in
    let s = make_instr (f a b) in
    attach_label aself (attach_label bself s)

module Asm__ = struct
    let dat l =
        let rec do_one = function
            | AsmStr s ->
                let t = ref [] in
                for i = String.length s - 1 downto 0 do
                    t := int_of_char s.[i] :: !t
                done;
                make_instr (DatStr (ENum 1, !t))
            | AsmBlank ->
                Empty 1
            | AsmTimes (c,e) ->
                let (c,cself) =
                    match parse_asmexpr c with
                    | Lit c', cself -> (c', cself)
                    | _, _ -> failwith "invalid values for DAT" in
                let (e,eself) =
                    match do_one e with
                    | Labeled (l,e') -> (e', Some l)
                    | e' -> (e', None) in
                let s =
                    match c, e with
                    | ENum c, Empty v ->
                        Empty (v*c)
                    | ENum c, Static cs ->
                        Static (repeat c cs)
                    | ENum c, Dynamic (Dat (c',e),min,max) ->
                        Dynamic (Dat (EMul (ENum c, c'), e), c * min, c * max)
                    | ENum c, Dynamic (DatStr (c',s),min,max) ->
                        Dynamic (DatStr (EMul (ENum c, c'), s), c * min, c * max)
                    | ce, Empty v ->
                        Dynamic (Res (EMul (ENum v, ce)), 0, 65535)
                    | ce, Static cs ->
                        Dynamic (DatStr (ce,cs), 0, 65536)
                    | _, _ -> failwith "TODO"
                in attach_label cself (attach_label eself s)
            | a ->
                let build = function
                    | Lit e -> Dat (ENum 1, e)
                    | _ -> failwith "invalid values for DAT"
                in make_unary_instr build a
        in match l with
        | [a] -> do_one a
        | l -> Blocked (List.map do_one l)

    let set  = make_binary_instr (fun a b -> Set (a,b))
    let add  = make_binary_instr (fun a b -> Add (a,b))
    let sub  = make_binary_instr (fun a b -> Sub (a,b))
    let mul  = make_binary_instr (fun a b -> Mul (a,b))
    let div  = make_binary_instr (fun a b -> Div (a,b))
    let mod_ = make_binary_instr (fun a b -> Mod (a,b))
    let shl  = make_binary_instr (fun a b -> Shl (a,b))
    let shr  = make_binary_instr (fun a b -> Shr (a,b))
    let and_ = make_binary_instr (fun a b -> And (a,b))
    let bor  = make_binary_instr (fun a b -> Bor (a,b))
    let xor  = make_binary_instr (fun a b -> Xor (a,b))
    let ife  = make_binary_instr (fun a b -> Ife (a,b))
    let ifn  = make_binary_instr (fun a b -> Ifn (a,b))
    let ifg  = make_binary_instr (fun a b -> Ifg (a,b))
    let ifb  = make_binary_instr (fun a b -> Ifb (a,b))
    let jsr  = make_unary_instr  (fun a   -> Jsr a)

    let label l s = Labeled (l,s)

    let block = function
        | [] -> Nothing
        | ss -> Blocked ss

    (* various pseudo-instructions *)
    let org v =
        let v = v land 0xffff in
        let label = gen_selflabel () in
        (* ORG v => %label: DAT (v - %label) TIMES _ *)
        Labeled (label,
            Dynamic (Res (ESub (ENum v, ELabel label)), 0, v))

    let align v =
        if v <= 0 || v >= 0x10000 then
            failwith ("invalid alignment " ^ string_of_int v)
        else if v = 1 then
            Blocked [] (* ALIGN 1 does not have any effect *)
        else
            let label = gen_selflabel () in
            (* ALIGN v => %label: DAT ((v - %label MOD v) MOD v) TIMES _ *)
            Labeled (label,
                Dynamic (Res (EMod (ESub (ENum v, EMod (ELabel label, ENum v)),
                                    ENum v)),
                         0, v - 1))

    let nop = Static [0x0001] (* SET A, A *)
    let jmp = make_unary_instr (fun a -> Jmp a)

    let is_stack_operand = function
        | AsmPush | AsmPeek | AsmPop -> true
        | _ -> false

    let push a =
        if is_stack_operand a then
            failwith "PUSH with a stack operand will act in an unexpected way; \
                      use \"SET PUSH, <operand>\" if you really want it.";
        make_unary_instr (fun a -> Set (Push, a)) a

    let pop a =
        if is_stack_operand a then
            failwith "POP with a stack operand will act in an unexpected way; \
                      use \"SET <operand>, POP\" if you really want it.";
        make_unary_instr (fun a -> Set (Push, a)) a

    let ret = Static [0x61c1] (* SET PC, POP *)
    let brk = Static [0x85c3] (* SUB PC, 1 *)
    let hlt = Static [0x85c3] (* SUB PC, 1 *)
end

(**********************************************************************)
(* Assembler. *)

let show_mapping mapping =
    Hashtbl.iter (fun k v -> Printf.printf "%s -> %#x\n" k v) mapping;
    Printf.printf "\n"

let resolve_labels origin oldmapping ss =
    let newmapping = Hashtbl.create 4 in

    (* newmapping first, oldmapping next. newmapping is required for resolving
     * instructions like "%self: DAT (0x100-%self) TIMES _" which will require
     * the position of %self in the very first pass. *)
    let resolve l =
        try
            ENum (Hashtbl.find newmapping l)
        with Not_found ->
            try
                ENum (Hashtbl.find oldmapping l)
            with Not_found ->
                ELabel l
    in

    let rec loop cur resolved acc cont =
        if cur > 0xffff then
            failwith "asm: the address exceeds the memory space";
        function
        | [] -> cont cur resolved acc
        | Nothing :: t ->
            loop cur resolved acc cont t
        | Empty v :: t ->
            let acc = Empty v :: acc in
            loop (cur + v) resolved acc cont t
        | Static cs :: t ->
            let cur = cur + List.length cs in
            let acc = Static cs :: acc in
            loop cur resolved acc cont t

        | Dynamic (i,min,max) :: t ->
            (* it is possible that the length of the instruction does not
             * change and still it can change depending on the label mapping.
             * if it *does* survive this pass then the instruction will have to
             * contain an unknown (free) label, which we can collect later. *)
            if min = max then
                let acc = Dynamic (i,min,max) :: acc in
                loop (cur + max) resolved acc cont t
            else
                (* we now evaluate the instruction and see if the instruction
                 * has been resolved or at least does not change its length. *)
                let ins = eval_instr_with_pc cur resolve i in
                begin match compile_instr ins with
                | Done cs ->
                    let cur = cur + List.length cs in
                    let acc = Static cs :: acc in
                    loop cur resolved acc cont t
                | DoneEmpty v ->
                    let acc = Empty v :: acc in
                    loop (cur + v) resolved acc cont t
                | NotYet (min,max) ->
                    (* if the length of this instruction does change then we
                     * assume the maximum length. *)
                    let acc = Dynamic (ins,min,max) :: acc in
                    loop (cur + max) resolved acc cont t
                end

        | Labeled (l,s) :: t ->
            (* old mapping and new mapping should match each other in order to
             * be fully resolved. *)
            if Hashtbl.mem newmapping l then
                failwith ("asm: duplicate label %" ^ l);
            let unchanged =
                try Hashtbl.find oldmapping l = cur
                with Not_found -> true in
            Hashtbl.replace newmapping l cur;
            let acc = Labeled (l, Blocked []) :: acc in
            loop cur (resolved && unchanged) acc cont (s::t)

        | Blocked ss :: t ->
            let cont' cur resolved acc = loop cur resolved acc cont t in
            loop cur resolved acc cont' ss
    in

    let cont last resolved acc = (resolved, newmapping, List.rev acc) in
    loop origin true [] cont ss

let remap_and_flatten resolve remap_label remap_instr ss =
    let id s = s in

    let remap_head = function
        | Dynamic (i,min,max) :: t ->
            let ins = eval_instr resolve i in
            begin match compile_instr ins with
            | Done cs -> Static cs :: t
            | DoneEmpty v -> Empty v :: t
            | NotYet (min,max) -> Dynamic (remap_instr ins,min,max) :: t
            end
        | ss -> ss
    in

    (* css: pending Static elements (to be added as soon as other stmt appears)
     * f: a function that attaches pending labels (to the next non-empty stmt)
     * acc: stmt accumulator, does not List.rev'ed automatically *)
    let rec inner_static css f acc ss =
        match remap_head ss with
        | Nothing :: t ->
            inner_static css f acc t
        | Static cs :: t ->
            inner_static (cs::css) f acc t
        | Blocked ss :: t ->
            inner_static css f acc (ss @ t)
        | Labeled (l,s) :: t when remap_label l = None ->
            inner_static css f acc (s::t)
        | ss ->
            let cs = List.concat (List.rev css) in
            if cs = [] then
                inner f acc ss
            else
                inner id (f (Static cs) :: acc) ss

    and inner_empty gap f acc ss =
        match remap_head ss with
        | Nothing :: t ->
            inner_empty gap f acc t
        | Empty v :: t ->
            inner_empty (gap+v) f acc t
        | Blocked ss :: t ->
            inner_empty gap f acc (ss @ t)
        | Labeled (l,s) :: t when remap_label l = None ->
            inner_empty gap f acc (s::t)
        | ss ->
            if gap = 0 then
                inner f acc ss
            else
                inner id (f (Empty gap) :: acc) ss

    and inner f acc ss =
        match remap_head ss with
        | [] ->
            (* adds a dummy stmt if there are pending labels at the end *)
            let remain = f Nothing in
            if remain = Nothing then acc else remain::acc
        | Nothing :: t ->
            inner f acc t
        | Empty v :: t ->
            inner_empty v f acc t
        | Static cs :: t ->
            inner_static [cs] f acc t
        | Dynamic (i,min,max) :: t ->
            inner id (f (Dynamic (i,min,max)) :: acc) t
        | Labeled (l,s) :: t ->
            let f = match remap_label l with
                | Some l' -> (fun s' -> f (Labeled (l',s')))
                | None -> f
            in inner f acc (s::t)
        | Blocked ss :: t ->
            inner f acc (ss @ t)

    in List.rev (inner id [] ss)

let localized_symbols = Hashtbl.create 64

let local locals ss =
    let mapping = Hashtbl.create 4 in
    let alloc_sym l =
        let gen =
            try
                Hashtbl.find localized_symbols l
            with Not_found ->
                let gen = gensym ("." ^ l) in
                Hashtbl.add localized_symbols l gen; gen
        in Hashtbl.add mapping l (gen ())
    in
    List.iter alloc_sym locals;

    let remap_label l =
        try Some (Hashtbl.find mapping l)
        with Not_found -> Some l in
    let resolve l =
        try ELabel (Hashtbl.find mapping l)
        with Not_found -> ELabel l in
    Blocked (remap_and_flatten resolve remap_label (fun i -> i) ss)

let asm ?(origin=0) ?(maxpass=50) ss =
    let rec pass oldmapping limit =
        if limit = 0 then
            failwith ("asm: cannot resolve all symbols after " ^
                      string_of_int maxpass ^ " pass(es)")
        else
            let (resolved, newmapping, ss') =
                resolve_labels origin oldmapping ss in
            if resolved then
                (newmapping, ss')
            else
                pass newmapping (limit-1)
    in

    (* we always run at least two passes. if the second pass results in the same
     * mapping as the first pass then the second pass succeeds. *)
    let (_, initmapping, _) = resolve_labels origin (Hashtbl.create 0) ss in
    let (mapping, ss') = pass initmapping maxpass in
    (*show_mapping mapping;*)

    (* remove all the local labels from the resulting code. also forces a longer
     * encoding for remaining expressions. note that if the expression is
     * explicitly order to use a shorter encoding via SHORT it won't change. *)
    let remap_label l =
        if String.length l > 0 && l.[0] = '.' then None else Some l in
    let resolve l =
        try ENum (Hashtbl.find mapping l)
        with Not_found -> ELabel l in
    Blocked (remap_and_flatten resolve remap_label force_longer_instr ss')

let to_words s =
    let off = ref 0 in
    let size = ref 64 in
    let arr = ref (Array.make !size 0) in

    let rec copy off size arr = function
        | [] -> (off, size, arr)
        | h::t ->
            if size = off then begin
                (* grow the array *)
                let newarr = Array.make (2*size) 0 in
                Array.blit arr 0 newarr 0 size;
                newarr.(off) <- h;
                copy (off+1) (2*size) newarr t
            end else begin
                arr.(off) <- h;
                copy (off+1) size arr t
            end
    in

    let skip gap =
        let rec nextsize target size =
            if target <= size then size else nextsize target (2*size) in
        if !off + gap <= !size then
            ()
        else begin
            let size' = nextsize (!off + gap) !size in
            let arr' = Array.make size' 0 in
            Array.blit !arr 0 arr' 0 !size;
            size := size'; arr := arr'
        end;
        off := !off + gap
    in

    let rec emit gap = function
        | [] -> ()
        | Nothing :: t -> emit gap t
        | Empty v :: t -> emit (gap+v) t
        | Static cs :: t ->
            skip gap;
            let (off',size',arr') = copy !off !size !arr cs in
            off := off'; size := size'; arr := arr'; emit 0 t
        | Dynamic (i,min,max) :: t ->
            failwith "to_words: cannot convert Dynamic stmt to an word"
        | Labeled (l,s) :: t -> emit gap (s::t)
        | Blocked ss :: t -> emit gap (ss @ t)
    in emit 0 [s]; Array.sub !arr 0 !off

let to_binary_le s =
    let words = to_words s in
    let nwords = Array.length words in
    let binary = String.create (2 * nwords) in
    for i = 0 to nwords - 1 do
        let w = words.(i) in
        binary.[2*i] <- char_of_int (w land 0xff);
        binary.[2*i+1] <- char_of_int (w lsr 8)
    done;
    binary

let to_binary_be s =
    let words = to_words s in
    let nwords = Array.length words in
    let binary = String.create (2 * nwords) in
    for i = 0 to nwords - 1 do
        let w = words.(i) in
        binary.[2*i] <- char_of_int (w lsr 8);
        binary.[2*i+1] <- char_of_int (w land 0xff)
    done;
    binary

