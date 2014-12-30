(*
 * =====================================================================================
 *
 *     Filename:  intermediary.ml
 *
 *  Description:  Print intermediary to C 
 *
 *     Author:  Adriaan Larmuseau, ajhl
 *    Company:  Uppsala IT
 *
 * =====================================================================================
 *)

(* Exceptions *) 
exception Cannot_convert_intermediary of string

(* ===  MODULE  ======================================================================
 *         Name:  CIntermediary
 *  Description:  defines and prints into the definitions of miniml.h
 * =====================================================================================
 *)
module type CINTERMEDIARY =
sig

 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)

  type type_u = TyInt | TyIgnore | TyBool | TyArrow of type_u * type_u 
    | TyStar of type_u * type_u | TyModule of type_u * type_u  | TyValue of type_u * type_u 
    | TyDeclar of type_u * type_u | TyFunctor of type_u * type_u * type_u
    | TySignature of type_u list | TyAbstract of type_u |TyCString of string | TyCType of string
    | TyCStruct of string

  type tempc = ToBValue of tempc | ToIValue of tempc | ToInt of tempc | ToBoolean of tempc | CVar of string 
    | ToQuestion of tempc * tempc * tempc | ToPair of tempc * tempc | ToComma of tempc * tempc
    | Assign of tempc * tempc | ToCall of tempc * tempc list | ToLambda of tempc | CInt of int
    | ToEnv of tempc | ToMod of tempc | Insert of tempc * tempc * tempc | ToCast of datastr * tempc
    | ToClosure of tempc * tempc * tempc | Get of tempc * tempc | CString of string | CastMAX of tempc
    | MALLOC of datastr *tempc * tempc | Ptr of tempc | Adress of tempc | ToByte of tempc 
    | ToOper of string * tempc * tempc | ToLeft of tempc | ToRight of tempc | Sizeof of datastr
    | ToStatic of type_u * tempc | Emptyline | ToReturn of tempc | ToDef of tempc * tempc * tempc list
    | InsertMeta of tempc * tempc * tempc * int * type_u  
    | CLocal of locality | Include of string | Comment of string
    | ToStructure of string * tempc list | Member of type_u * string 
    | CallMember of type_u * string * type_u list | SetMember of string * string * tempc 
    | ToFunctor of tempc | GetStr of tempc * tempc 

  and locality = LOCAL | SECRET | FUNCTIONALITY | ENTRYPOINT
  and datastr = VALUE | BINDING | STRUCTURE | VOID | DATA | DTYPE | CHAR | MODULE | MODDATA | ACC |FIELD
  and consts = ENV | ARG | MOD | STR | TOP
  and calls = BOOT | CONV | CONT | STRCPY | PATH | PATHV
  and headers = MINI | ENTRY
  and accs = BVAL | BMOD
  type args = (datastr * consts) list
  type funcdef = locality * type_u * string * args * bool


 (*-----------------------------------------------------------------------------
  *  Functions
  *-----------------------------------------------------------------------------*)

  val printty : type_u -> string

  val printc : tempc -> string

  val printl : locality -> string

  val printd : datastr -> string

  val printf : funcdef -> string

  val printa : accs -> string

  val printconst : consts -> string
  
  val constv : consts -> tempc

  val consta : accs -> tempc

  val constd : datastr -> type_u

  val constc : calls -> tempc

  val consth : headers -> tempc

  val format : int -> string list -> string list

  val range : int -> int -> int list

  val func_end : string list

  val header : string list -> string list

  val footer : string list

  val separate : string -> string list -> string list


 (*-----------------------------------------------------------------------------
  *  Global constants - TODO remove
  *-----------------------------------------------------------------------------*)

  val int_op : string list
  val var_prefix : string

end

module CIntermediary : CINTERMEDIARY =
struct

 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)

  type type_u = TyInt | TyIgnore | TyBool | TyArrow of type_u * type_u 
    | TyStar of type_u * type_u | TyModule of type_u * type_u  | TyValue of type_u * type_u 
    | TyDeclar of type_u * type_u | TyFunctor of type_u * type_u * type_u
    | TySignature of type_u list | TyAbstract of type_u |TyCString of string | TyCType of string
    | TyCStruct of string  

  type tempc = ToBValue of tempc | ToIValue of tempc | ToInt of tempc | ToBoolean of tempc | CVar of string 
    | ToQuestion of tempc * tempc * tempc | ToPair of tempc * tempc | ToComma of tempc * tempc
    | Assign of tempc * tempc | ToCall of tempc * tempc list | ToLambda of tempc | CInt of int
    | ToEnv of tempc | ToMod of tempc | Insert of tempc * tempc * tempc | ToCast of datastr * tempc
    | ToClosure of tempc * tempc * tempc  | Get of tempc * tempc | CString of string | CastMAX of tempc
    | MALLOC of datastr *tempc * tempc | Ptr of tempc | Adress of tempc | ToByte of tempc 
    | ToOper of string * tempc * tempc | ToLeft of tempc | ToRight of tempc | Sizeof of datastr
    | ToStatic of type_u * tempc | Emptyline | ToReturn of tempc | ToDef of tempc * tempc * tempc list
    | InsertMeta of tempc * tempc * tempc * int * type_u 
    | CLocal of locality | Include of string | Comment of string
    | ToStructure of string * tempc list | Member of type_u * string  
    | CallMember of type_u * string * type_u list | SetMember of string * string * tempc
    | ToFunctor of tempc | GetStr of tempc * tempc

  and locality = LOCAL | SECRET | FUNCTIONALITY | ENTRYPOINT

  and datastr = VALUE | BINDING | STRUCTURE | VOID | DATA | DTYPE | CHAR | MODULE | MODDATA | ACC |FIELD

  and consts = ENV | ARG | MOD | STR | TOP

  and calls = BOOT | CONV | CONT | STRCPY | PATH | PATHV

  and headers = MINI | ENTRY

  and accs = BVAL | BMOD

  and args = (datastr * consts) list

  and funcdef = locality * type_u * string * args * bool


 (*-----------------------------------------------------------------------------
  *  Helper Funcions
  *-----------------------------------------------------------------------------*)

  let _ = Random.self_init()
  let gen_rand length =
    let gen() = match Random.int(26+26+10) with
        n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26 in
      let genstr _ = String.make 1 (char_of_int(gen())) in
      "_"^(String.concat "" (Array.to_list (Array.init length genstr)))

  (* print locality *)
  let printl = function
    | LOCAL -> "LOCAL"
    | SECRET -> "SECRET"
    | FUNCTIONALITY -> "FUNCTIONALITY"
    | ENTRYPOINT -> "ENTRYPOINT"

  (* print data structure *)
  let printd = function
    | VALUE -> "VALUE" 
    | BINDING -> "BINDING*"
    | STRUCTURE -> "STRUCTURE"
    | VOID -> "void"
    | DATA -> "DATA"
    | DTYPE -> "DTYPE"
    | CHAR -> "char"
    | ACC -> "ACC"
    | FIELD -> "FIELD"
    | MODULE -> "MODULE"
    | MODDATA -> "MODDATA"

  (* print constants *)
  let printconst = function
    | ENV -> "my_env" 
    | ARG -> "my_arg"
    | MOD -> "my_mod" 
    | STR -> "my_str"
    | TOP -> "toplevel"

  (* print headers *)
  let printh = function
    | MINI -> "miniml.h"
    | ENTRY -> "entry.h"

  (* print accessors *)
  let printa = function
    | BVAL -> "BVAL"
    | BMOD -> "BMOD"

  (* print functions to be used from the headers *)
  let printcalls = function
    | STRCPY -> "str_cpy"
    | BOOT -> "bootup"
    | CONV -> "convertV"
    | CONT -> "convertT"
    | PATH -> "path_module"
    | PATHV -> "path_value"

  (* build cvar from const *)
  let constv v = CVar (printconst v)

  let constd v = TyCType (printd v)

  let constc v = CVar (printcalls v)

  let consta v = CVar (printa v)

  let consth v = Include (printh v)

  (* range operator *)
  let range i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc) in 
    (aux j [])

  (* add ; and indentation *)
  let format n ls = 
    let indent = (String.concat "" (List.map (fun x -> " ") (range 1 n))) in
      (List.map (fun x  -> if (not (x = "")) then (indent ^ x ^ ";") else "" ) ls) 


  (* end of function *)
  let func_end = ("}\n"::[]) 

  (* separate name *)
  let separate name = function [] -> []
      | x::xs as ls -> let st = "----------------------" in
        ["//"^ st ^ " " ^ name ^ " " ^ st ; ""] @ ls @ [""] 
  
  (* header *)
  let header ls = ["// Compiled by lanren"] @ ls @ [""] 

  (* footer *)
  let footer = ["// Include the entrypoints & binding code"; 
                "#include \"binding.c\""; 
                "#include \"data.c\"" ; 
                "#include \"entry.c\""; ""] 


 (*-----------------------------------------------------------------------------
  *  Global constants
  *-----------------------------------------------------------------------------*)

  let int_op = ["+"; "-"; "/"; "*"]
  and var_prefix = (gen_rand 6)


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  printty
  *  Description:  When all is said and done, conver the computation to string
  * =====================================================================================
  *)
  let rec printty : type_u -> string = function TyIgnore -> "TIgnore"
    | TyInt -> "TInt"
    | TyBool -> "TBoolean"
    | TyArrow (a,b) -> "makeTArrow("^(printty a)^","^(printty b)^")"
    | TyStar (a,b) -> "makeTStar("^(printty a)^","^(printty b)^")"
    | TySignature ls -> 
      let rec conversion = function x::[] -> "makeTSignature("^(printty x)^")" 
          | x :: xs ->  "chainTSignature("^(conversion xs)^","^(printty x)^")"
          | _ -> raise (Cannot_convert_intermediary "Signature List cannot be empty !")
        in 
        conversion ls
    | TyModule (n,ty) -> "makeTModule("^(printty n)^","^(printty ty)^")"
    | TyDeclar (n,a) -> "makeTDeclaration("^(printty n)^","^(printty a)^")"
    | TyValue (n,a) -> "makeTValue("^(printty n)^","^(printty a)^")"
    | TyFunctor (n,a,b) -> "makeTFunctor("^(printty n)^","^(printty a)^","^(printty b)^")"
    | TyAbstract n -> "makeTAbstract("^(printty n)^")"
    | TyCString n -> "\""^n^"\""
    | TyCType s -> s
    | TyCStruct s -> "struct "^s


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  printc
  *  Description:  When all is said and done, conver the computation to string
  * =====================================================================================
  *)
  let rec printc = function ToBValue a -> (printc a)^".b.value"  (* TODO is ptr ? *)
    | ToIValue a -> (printc a)^".i.value"
    | ToInt a -> "makeInt(" ^ (printc a) ^ ")"
    | ToBoolean a -> "makeBoolean("^ (printc a) ^ ")"
    | ToQuestion(a,b,c) -> "("^(printc a)^" ? "^(printc b)^" : "^(printc c)^")"
    | ToPair (a,b) -> "makePair("^(printc a)^","^(printc b)^")"
    | ToComma(a,b) -> "("^(printc a)^","^(printc b)^")"
    | Assign (a,b) -> (printc a) ^ "=" ^ (printc b)
    | CVar str -> str
    | CInt a -> (string_of_int a)
    | CString str -> ("\""^str^"\"")
    | ToCall (f,ls) -> (printc f)^"("^(String.concat "," (List.map printc ls))^")"
    | ToLambda a -> (printc a)^".c.lam" (* TODO is ptr ? *)
    | ToEnv a -> (printc a)^".c.env"
    | ToMod a -> (printc a)^".c.mod"
    | CastMAX a -> "(MAX) "^(printc a)
    | Insert (a,b,c) -> "insertBinding("^(printc (Adress a))^","^(printc b)^","^(printc c)^")"
    | ToClosure (a,b,c) -> "makeClosure("^(printc a)^","^(printc b)^","^(printc c)^")"
    | Get (a,b) -> "getValue("^(printc a)^","^(printc b)^")" 
    | GetStr (a,b) -> "getModule("^(printc a)^","^(printc b)^")"
    | MALLOC (a,b,c) -> (printd a)^" "^(printc (Ptr b))^" = malloc("^(printc c)^")"
    | Ptr a -> "*"^(printc a) 
    | Adress a -> "&"^(printc a)
    | ToByte a -> (printc a)^".byte"
    | ToOper (a,b,c) -> (printc b) ^" "^a^" "^(printc c)
    | ToLeft a -> "(*("^ (printc a) ^ ".p.left))"
    | ToRight a -> "(*("^ (printc a) ^ ".p.right))"
    | Sizeof a -> "sizeof("^ (printd a)^")"
    | ToCast (a,b) -> "((" ^ (printd a) ^")"^(printc b)^")"
    | ToStatic (a,b) -> "static "^(printty a)^" "^(printc b)
    | Emptyline -> ""
    | ToReturn a -> "return "^(printc a)
    | ToDef (a,b,ls) -> "LOCAL "^(printc a)^" "^(printc b)^"("^(match ls with [] -> "void"
      | _ -> (String.concat ";" (List.map printc ls)))^"){\n"
    | ToStructure (s,ls) -> "struct "^s^" {"^ (String.concat " "  (List.map printc ls))^"}"
    | ToFunctor a -> "("^(printc a)^").f.Functor"
    | Member (ty,str) -> (printty ty)^" "^str^";"
    | CallMember (ret,n,arg) -> (printty ret)^" (*"^n^")("^(match arg with [] -> "void" 
      | ls -> (String.concat "," (List.map printty arg)))^");"
    | SetMember (stra,strb,a) -> stra^"."^strb^" = "^(printc a) 
    | CLocal a -> (printl a)
    | Include a -> "#include \""^a^"\""
    | InsertMeta (a,b,c,d,e) -> "insertBigBinding("^(printc (Adress a))^","^(printc b)^","^(printc c)^","^
      (printc (CInt d))^","^(printty e)^")"
    | Comment a -> "/* "^a^"*/"


  (* print functions *)
  let printf = function
    | (loc,ret,name,args,def) -> let par = (match args with
        | [] -> "void"
        | ls -> (String.concat "," (List.map (function (x,y) -> (printd x)^" "^(printconst y)) args))) in
        ((printl loc)^" "^(printty ret)^" "^name^" "^"("^par^")"^(if def then ";" else"{"))

end


