(* dcputhings: Assorted Tools for DCPU-16 Assembly.
 * Written by Kang Seonghoon. See LICENSE for the full license statement.
 *)

let hexdump ?(origin=0) s =
    let words = DcpuAsm.to_words s in
    let size = Array.length words in
    let linestart = origin / 8 in
    let lineend = (origin + size - 1) / 8 in
    for i = linestart to lineend do
        Printf.printf "%04x:" (i*8);
        for j = (i*8) - origin to (i*8+7) - origin do
            if j < 0 || j >= size then
                Printf.printf " ...."
            else
                Printf.printf " %04x" words.(j)
        done;
        Printf.printf "\n"
    done;
;;

let hexdump_asm ?(origin=0) ?(showasm=false) label ss =
    Printf.printf "* %s\n\n" label;
    if showasm then begin
        List.iter DcpuAsm.print_stmt ss;
        Printf.printf "\n"
    end;
    hexdump ~origin:origin (ASM ~origin:origin ss);
    Printf.printf "\n\n"
;;

(**********************************************************************)
(* Notch's codes. Copyright (c) 2012 Mojang AB. *)

(* converted from http://0x10c.com/doc/dcpu-16.txt *)
hexdump_asm "Notch's quick example" [
    (* Try some basic stuff *)
    SET A, 0x30;
    SET [0x1000], 0x20;
    SUB A, [0x1000];
    IFN A, 0x10;
        SET PC, %crash;

    (* Do a loopy thing *)
    SET I, 10;
    SET A, 0x2000;
%loop:
    SET [0x2000+I], [A];
    SUB I, 1;
    IFN I, 0;
        SET PC, %loop;

    (* Call a subroutine *)
    SET X, 0x4;
    JSR %testsub;
    SET PC, %crash;

%testsub:
    SHL X, 4;
    SET PC, POP;

    (* Hang forever. X should now be 0x40 if everything went right. *)
%crash:
    SET PC, %crash;
];;

hexdump_asm "Notch's quick example (with a longer encoding)" [
    (* Try some basic stuff *)
    SET A, 0x30;
    SET [0x1000], 0x20;
    SUB A, [0x1000];
    IFN A, 0x10;
        SET PC, LONG %crash;

    (* Do a loopy thing *)
    SET I, 10;
    SET A, 0x2000;
%loop:
    SET [0x2000+I], [A];
    SUB I, 1;
    IFN I, 0;
        SET PC, LONG %loop;

    (* Call a subroutine *)
    SET X, 0x4;
    JSR LONG %testsub;
    SET PC, LONG %crash;

%testsub:
    SHL X, 4;
    SET PC, POP;

    (* Hang forever. X should now be 0x40 if everything went right. *)
%crash:
    SET PC, LONG %crash;
];;

(* converted from http://i.imgur.com/XIXc4.jpg *)
hexdump_asm "Hello world from Notch" [
(* Assembler test for DCPU *)
(* by Markus Persson *)

%start:
    SET I, 0;
    SET J, 0;
    SET B, 0xf100;

%nextchar:
    SET A, [%data+I];
    IFE A, 0;
        SET PC, %eof;
    IFG A, 0xff;
        SET PC, %setcolor;
    BOR A, B;
    SET [0x8000+J], A;
    ADD I, 1;
    ADD J, 1;
    SET PC, %nextchar;

%setcolor:
    SET B, A;
    AND B, 0xff;
    SHL B, 8;
    IFG A, 0x1ff;
        ADD B, 0x80;
    ADD I, 1;
    SET PC, %nextchar;

%data:
    DAT 0x170, "Hello ", 0x2e1, "world", 0x170, ", how are you?";

%eof:
    SET PC, %start;
];;

(**********************************************************************)
(* Fixed point arithmetic by Entroper *)
(* https://github.com/Entroper/DCPU-16-fixedmath *)

let fxaddd (b,a) (y,x) t =
    BLOCK [
        ADD #a, #x;
        ADD #b, O;
        SET #t, O;      (* see if the carry carried *)
        ADD #b, #y;     (* if it did, this won't *)
        BOR #t, O;      (* set O in either case *)
        SET O, #t;
    ] in

let fxsubd (b,a) (y,x) t =
    BLOCK [
        SUB #a, #x;
        ADD #b, O;      (* if this doesn't carry, then we borrowed from 0 *)
        SET #t, O;
        SUB #t, 1;      (* C is now 0xFFFF if we borrowed from 0, 0x0000 otherwise *)
        SUB #b, #y;
        BOR #t, O;
        SET O, #t;
    ] in

let fxmuld (b,a) (y,x) t1 t2 t3 =
    (*           BBBB AAAA
     *         * YYYY XXXX
     * -------------------
     *           XXXX*AAAA
     *      XXXX*BBBB
     *      YYYY*AAAA
     * YYYY*BBBB
     * -----------------
     * OOOO BBBB AAAA xxxx
     *)
    BLOCK [
        SET #t1, #a;    (* we can't lose A yet *)
        MUL #t1, #x;    (* X*A *)
        SET #t2, O;     (* save overflow of X*A for second column *)
        MUL #a, #y;     (* Y*A (duh), and begin accumulating in A *)
        SET #t3, O;     (* save overflow of Y*A for third column, begin accumulating in t3 *)
        ADD #a, #t2;    (* accumulate overflow of X*A, we can now reuse t2 *)
        ADD #t3, O;     (* potential carry from the second column *)
                        (* the above cannot produce a propagating carry *)
        MUL #x, #b;     (* X*B (duh) *)
        ADD #t3, O;     (* accumulate overflow of X*B for third column *)
        SET #t2, O;     (* which can produce a carry *)
        ADD #a, #x;     (* accumulate X*B *)
        ADD #t3, O;     (* this can carry again, and we accumulate the third column in t3 *)
        ADD #t2, O;     (* potential carry from third column *)
        MUL #b, #y;     (* Y*B, begin accumulating in B *)
        ADD #t2, O;     (* add overflow of Y*B to fourth column *)
                        (* fourth column cannot overflow *)
        ADD #b, #t3;    (* accumulate the rest of the third column *)
        ADD #t2, O;     (* potential carry *)
        SET O, #t2;     (* set O *)
    ] in

let ldd (b,a) label =
    BLOCK [
        SET #b, [#label];
        SET #a, [#label+1];
    ] in

let datd v =
    DAT VAL (v / 10000), VAL (v mod 10000) SHL 16 DIV 10000
in

hexdump_asm "Fixed point arithmetic" [
    (* calculates 80 - (2.5 + 4.7) * 9.9.
     * the accurate result: X=0x0008, Y=0xb852 *)
    ldd (A,B) %a;
    ldd (X,Y) %b;
    fxaddd (A,B) (X,Y) C;
    ldd (X,Y) %c;
    fxmuld (A,B) (X,Y) C I J;
    ldd (X,Y) %d;
    fxsubd (X,Y) (A,B) C;
    SUB PC, 1;

%a: datd  25000;
%b: datd  47000;
%c: datd  99000;
%d: datd 800000;
];;

(**********************************************************************)
(* Macro facility *)

let switch e cases0 =
    let cases = Hashtbl.create 4 in
    let minv = ref max_int in
    let maxv = ref min_int in
    List.iter (fun (v,case) -> minv := min !minv v;
                               maxv := max !maxv v;
                               Hashtbl.add cases v case) cases0;
    let minv = !minv in
    let maxv = max minv !maxv in
    if minv < 0 || maxv > 0xffff then
        failwith "switch: out of bound case values";

    let labels = ref ["_skip"; "_jmptab"] in
    let jmptab = ref [] in
    let body = ref [] in
    for v = minv to maxv do
        try
            let case = Hashtbl.find cases v in
            let label = "_case" ^ string_of_int v in
            labels := label :: !labels;
            jmptab :=
                (
                    DAT %#label
                ) :: !jmptab;
            body :=
                List.rev_append [
                %#label:
                    BLOCK case;
                    (* do not add redundant jumps at the end *)
                    if v = maxv then
                        PASS
                    else
                        JMP %_skip;
                ] !body
        with Not_found ->
            jmptab := DAT %_skip :: !jmptab
    done;

    BLOCK LOCAL #(!labels) [
        if minv > 0 then
            SUB #e, minv
        else
            PASS;
        SET O, %_skip;          (* abusing O as a temporary storage *)
        IFG #e, VAL (maxv-minv);
            JMP O;              (* since "JMP %_skip" may not fit in a word *)
        JMP [#e+%_jmptab];
    %_jmptab:
        BLOCK (List.rev !jmptab);
        BLOCK (List.rev !body);
    %_skip:
    ] in

hexdump_asm "Macro facility" [
    SET A, 8;
    SET X, 0;
    switch A [
        3, [ SET X, 1; SET B, 8 ];
        9, [ SET X, 3; SET B,-1 ];
        8, [ SET X, 2; SET B, 9 ];
    ];
    (* now X=2, B=9 (and A is clobbered) *)
    SET Y, 0;
    switch B [
        3, [ SET Y, 1; SET C, 8 ];
        9, [ SET Y, 3; SET C,-1 ];
        8, [ SET Y, 2; SET C, 9 ];
    ];
    (* now Y=3, C=3 (and B is clobbered) *)
    SET Z, 0;
    switch C [
        3, [ SET Z, 1; SET I, 8 ];
        9, [ SET Z, 3; SET I,-1 ];
        8, [ SET Z, 2; SET I, 9 ];
    ];
    (* now Z=0. *)
    SUB PC, 1;
];;

(**********************************************************************)
(* Some other examples. *)

hexdump_asm "Shortcuts" [
    JMP %next;                  (* same as SET PC, %next; *)
%next:
    PUSH A;                     (* same as SET PUSH, A; *)
    SET B, [SP];                (* same as SET B, PEEK; *)
    POP C;                      (* same as SET C, POP; *)
    BRK;                        (* same as SUB PC, 1; for now.
                                 * may change if we get de jure BRK command. *)
    HLT;                        (* same as BRK; for now. *)
];;

let screen_base = 0x8000 in
hexdump_asm "Immediate expressions" [
    SET A, 0x8000;                          (* immediate *)
    SET A, '*';                             (* immediate (char code) *)
    SET A, B;                               (* register *)
    SET A, [B];                             (* memory ref. *)
    SET A, [B+0x1000];                      (* indexed addressing *)
    SET A, [0x1000+B];                      (* the order does not matter *)
    SET A, [B+(%datend-%dat) DIV 2];        (* arithmetics on labels, DIV *)
    SET A, [4*B - 3*B];                     (* some more arithmetics on regs.;
                                             * note that [2*B] is still error *)
    SET A, %dat * 7 MOD 0x1f;               (* MOD *)
    SET A, 1 SHL 11 SHR 3 - 1;              (* SHL, SHR *)
    SET A, (0x123 OR 0x321) AND (NOT 0xff); (* AND, OR, NOT *)
    SET A, 3 XOR 5;                         (* XOR *)
    SET A, VAL (String.length "hello?");    (* Ocaml code as an immediate *)
    SET A, PTR [X];                         (* longer form of memory ref. *)
    SET A, IMM (0x8000 - %datend);          (* longer form of immediate *)
    SET A, screen_base + 1;                 (* a single Ocaml identifier can be
                                             * used as an immediate *)
    BRK;

%dat:
    (* DAT supports number, string and Ocaml code as a string. *)
    DAT 42, 'A', "loha! ", STR (String.uppercase "woah");
%datend:
];;

hexdump_asm ~origin:0x0136 "LONG immediates, non-zero origin" [
%code:
    SET A, LONG 3;              (* while 3 fits in a value, it will emit
                                 * an additional word as requested. *)
    SET [%code], 3;             (* this will not emit an additional word. *)
    SET A, LONG %code;          (* if the immediate does not fit in a value
                                 * LONG is redundant. *)
];;

