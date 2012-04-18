(* dcputhings: Assorted Tools for DCPU-16 Development.
 * Written by Kang Seonghoon. See LICENSE for the full license statement.
 *)

(**********************************************************************)
(* Assembly AST. *)

(* DCPU-16 registers. not all of them supports the indexed addressing. *)
type reg = A | B | C | X | Y | Z | I | J | PC | SP | O
type label = string

val string_of_reg : reg -> string
val index_of_reg : reg -> int

(* generates unique labels with a given prefix. *)
val gensym : string -> unit -> label

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
    | EOr of expr * expr
    | EXor of expr * expr
    | EShl of expr * expr
    | EShr of expr * expr

val string_of_expr : expr -> string

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

val string_of_value : value -> string

type instr =
    | Dat of expr
    | DatStr of string
    | Set of value * value
    | Add of value * value
    | Sub of value * value
    | Mul of value * value
    | Div of value * value
    | Mod of value * value
    | Shl of value * value
    | Shr of value * value
    | And of value * value
    | Bor of value * value
    | Xor of value * value
    | Ife of value * value
    | Ifn of value * value
    | Ifg of value * value
    | Ifb of value * value
    | Jsr of value
    | Jmp of value

val string_of_instr : instr -> string

(**********************************************************************)
(* Assembly Expressions. (e.g. [A+0x42]) *)

type asmexpr (* opaque *)

(* internal code generator for pa_dcpuasm *)
module AsmExpr__ : sig
    val imm   : int -> asmexpr
    val long  : asmexpr -> asmexpr
    val short : asmexpr -> asmexpr
    val str   : string -> asmexpr
    val reg   : reg -> asmexpr
    val push  : asmexpr
    val peek  : asmexpr
    val pop   : asmexpr
    val label : label -> asmexpr
    val mem   : asmexpr -> asmexpr
    val neg   : asmexpr -> asmexpr
    val not_  : asmexpr -> asmexpr
    val add   : asmexpr -> asmexpr -> asmexpr
    val sub   : asmexpr -> asmexpr -> asmexpr
    val mul   : asmexpr -> asmexpr -> asmexpr
    val div   : asmexpr -> asmexpr -> asmexpr
    val mod_  : asmexpr -> asmexpr -> asmexpr
    val and_  : asmexpr -> asmexpr -> asmexpr
    val or_   : asmexpr -> asmexpr -> asmexpr
    val xor   : asmexpr -> asmexpr -> asmexpr
    val shl   : asmexpr -> asmexpr -> asmexpr
    val shr   : asmexpr -> asmexpr -> asmexpr
end

val parse_asmexpr : asmexpr -> value * label option

(**********************************************************************)
(* Assembly Statements. (e.g. SUB SP, 1) *)

type stmt =
    | Nothing of unit
    | Static of int list
    | Dynamic of instr * int * int
    | Labeled of label * stmt
    | Blocked of stmt list

val print_stmt : stmt -> unit

(* internal code generator for pa_dcpuasm *)
module Asm__ : sig
    val dat   : asmexpr list -> stmt
    val set   : asmexpr -> asmexpr -> stmt
    val add   : asmexpr -> asmexpr -> stmt
    val sub   : asmexpr -> asmexpr -> stmt
    val mul   : asmexpr -> asmexpr -> stmt
    val div   : asmexpr -> asmexpr -> stmt
    val mod_  : asmexpr -> asmexpr -> stmt
    val shl   : asmexpr -> asmexpr -> stmt
    val shr   : asmexpr -> asmexpr -> stmt
    val and_  : asmexpr -> asmexpr -> stmt
    val bor   : asmexpr -> asmexpr -> stmt
    val xor   : asmexpr -> asmexpr -> stmt
    val ife   : asmexpr -> asmexpr -> stmt
    val ifn   : asmexpr -> asmexpr -> stmt
    val ifg   : asmexpr -> asmexpr -> stmt
    val ifb   : asmexpr -> asmexpr -> stmt
    val jsr   : asmexpr -> stmt
    val nop   : stmt
    val jmp   : asmexpr -> stmt
    val label : string -> stmt -> stmt
    val block : stmt list -> stmt
end

(**********************************************************************)
(* Assembler. *)

(* also available in pa_dcpuasm as BLOCK LOCAL (with syntactic sugars) *)
val local : string list -> stmt list -> stmt

(* also available in pa_dcpuasm as ASM *)
val asm : ?origin:int -> ?maxpass:int -> stmt list -> stmt

val to_words : stmt -> int array
val to_binary_be : stmt -> string
val to_binary_le : stmt -> string

