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
    | TySignature of type_u list | TyAbstract of type_u |TyCString of string

  type tempc = ToBValue of tempc | ToIValue of tempc | ToInt of tempc | ToBoolean of tempc | CVar of string 
    | ToQuestion of tempc * tempc * tempc | ToPair of tempc * tempc | ToComma of tempc * tempc
    | Assign of tempc * tempc | ToCall of tempc * tempc list | ToLambda of tempc | CInt of int
    | ToEnv of tempc | ToMod of tempc | Insert of tempc * tempc * tempc | ToCast of string * tempc
    | ToClosure of tempc * tempc  | Get of tempc * tempc | CString of string | CastMAX of tempc
    | MALLOC of tempc *tempc * tempc | Ptr of tempc | Adress of tempc | ToByte of tempc 
    | ToOper of string * tempc * tempc | ToLeft of tempc | ToRight of tempc | Sizeof of tempc
    | ToStatic of tempc * string | Emptyline | ToReturn of tempc | ToDef of tempc * tempc * tempc list
    | InsertMeta of tempc * tempc * tempc * int * type_u
    | CLocal of locality

  and locality = LOCAL | SECRET | FUNCTIONALITY | ENTRYPOINT


 (*-----------------------------------------------------------------------------
  *  Functions
  *-----------------------------------------------------------------------------*)

  val printty : type_u -> string

  val printc : tempc -> string

  val printl : locality -> string

 (*-----------------------------------------------------------------------------
  *  Global constants
  *-----------------------------------------------------------------------------*)

  val const_env : string
  val const_arg : string
  val const_mod : string
  val const_str : string
  val int_op : string list
  val c_value : string
  val c_binding : string
  val c_strc : string 
  val c_strcpy : string
  val var_prefix : string
  val c_boot : string
  val c_topl : string

end

module CIntermediary : CINTERMEDIARY =
struct

 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)

  type type_u = TyInt | TyIgnore | TyBool | TyArrow of type_u * type_u 
    | TyStar of type_u * type_u | TyModule of type_u * type_u  | TyValue of type_u * type_u 
    | TyDeclar of type_u * type_u | TyFunctor of type_u * type_u * type_u
    | TySignature of type_u list | TyAbstract of type_u |TyCString of string

  type tempc = ToBValue of tempc | ToIValue of tempc | ToInt of tempc | ToBoolean of tempc | CVar of string 
    | ToQuestion of tempc * tempc * tempc | ToPair of tempc * tempc | ToComma of tempc * tempc
    | Assign of tempc * tempc | ToCall of tempc * tempc list | ToLambda of tempc | CInt of int
    | ToEnv of tempc | ToMod of tempc | Insert of tempc * tempc * tempc | ToCast of string * tempc
    | ToClosure of tempc * tempc  | Get of tempc * tempc | CString of string | CastMAX of tempc
    | MALLOC of tempc *tempc * tempc | Ptr of tempc | Adress of tempc | ToByte of tempc 
    | ToOper of string * tempc * tempc | ToLeft of tempc | ToRight of tempc | Sizeof of tempc
    | ToStatic of tempc * string | Emptyline | ToReturn of tempc | ToDef of tempc * tempc * tempc list
    | InsertMeta of tempc * tempc * tempc * int * type_u
    | CLocal of locality

  and locality = LOCAL | SECRET | FUNCTIONALITY | ENTRYPOINT


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

  let printl = function
    | LOCAL -> "LOCAL"
    | SECRET -> "SECRET"
    | FUNCTIONALITY -> "FUNCTIONALITY"
    | ENTRYPOINT -> "ENTRYPOINT"

 (*-----------------------------------------------------------------------------
  *  Global constants
  *-----------------------------------------------------------------------------*)

  let const_env = "my_env" 
  and const_arg = "my_arg"
  and const_mod = "my_mod" 
  and const_str = "my_str"
  and int_op = ["+"; "-"; "/"; "*"]
  and c_value = "VALUE"
  and c_binding = "BINDING*"
  and c_strc = "STRUCTURE"
  and c_strcpy = "str_cpy"
  and var_prefix = (gen_rand 6)
  and c_boot = "bootup"
  and c_topl = "toplevel"


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
    | ToClosure (a,b) -> "makeClosure("^(printc a)^","^(printc b)^")"
    | Get (a,b) -> "getValue("^(printc a)^","^(printc b)^")" 
    | MALLOC (a,b,c) -> (printc a)^" "^(printc (Ptr b))^" = malloc("^(printc c)^")"
    | Ptr a -> "*"^(printc a) 
    | Adress a -> "&"^(printc a)
    | ToByte a -> (printc a)^".byte"
    | ToOper (a,b,c) -> (printc b) ^" "^a^" "^(printc c)
    | ToLeft a -> "(*("^ (printc a) ^ ".p.left))"
    | ToRight a -> "(*("^ (printc a) ^ ".p.right))"
    | Sizeof a -> "sizeof("^ (printc a)^")"
    | ToCast (a,b) -> "((" ^ a ^")"^(printc b)^")"
    | ToStatic (a,b) -> "static char "^(printc (Assign ((Ptr a),(CString b))))
    | Emptyline -> ""
    | ToReturn a -> "return "^(printc a)
    | ToDef (a,b,ls) -> "LOCAL "^(printc a)^" "^(printc b)^"("^(match ls with [] -> "void"
      | _ -> (String.concat ";" (List.map printc ls)))^"){\n"
    | CLocal a -> (printl a)
    | InsertMeta (a,b,c,d,e) -> "insertBigBinding("^(printc (Adress a))^","^(printc b)^","^(printc c)^","^
      (printc (CInt d))^","^(printty e)^")"

end


