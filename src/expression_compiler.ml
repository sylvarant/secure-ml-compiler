(*
 * =====================================================================================
 *
 *     Filename:  expression_compiler.ml
 *
 *  Description:  Compile the AST into C
 *
 *     Author:  Adriaan Larmuseau, ajhl
 *    Company:  Uppsala IT
 *
 * =====================================================================================
 *)

open Mini
open Modules
open Intermediary
open Modules_compiler

(* Exceptions *) 
exception Cannot_compile of string

(* 
 * ===  MODULE  ======================================================================
 *         Name:  MiniMLComp
 *  Description:  The expression compiler
 * =====================================================================================
 *)
module MiniMLComp : EXPR_COMP =
struct

  module Intermediary = CIntermediary
  module MOmega = Omega(Intermediary)
  open MiniML 
  open CIntermediary
  open MOmega

 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)
  type exprcomp = Omega(Intermediary).compred list * Omega(Intermediary).computation


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  parse_computation
  *  Description:  compiles the lambda calculus
  * =====================================================================================
  *)
  let rec compile env path program = 

    let funclist = ref [] in

    (*TODO generate v@r based on existing syms *)
    let new_var = let count = ref (-1) in 
      fun () -> incr count; (var_prefix^"_v_r_" ^ (string_of_int !count)) in

    let new_lambda = let count = ref (-1) in
        fun () -> incr count; ("Lam_"^(string_of_int !count)) in

    let new_func str = (str^new_lambda())  in

    let new_ptr = let count = ref (-1) in
        fun () -> incr count; (var_prefix^"_ptr" ^ (string_of_int !count)) in

    (* get rid of the let terms *)
    let rec desugar : MiniML.term -> MiniML.term  = function
      | Let (id,e,t) -> Apply( (desugar (Function(id,ignore_type,t))) , (desugar e))
      | Constant _ as t -> t
      | Boolean _ as t -> t
      | Longident _ as t -> t
      | If (a,b,c) -> If ((desugar a),(desugar b),(desugar c))
      | Prim (op,ls) -> let nls = (List.map desugar ls) in Prim(op,nls)
      | Function(id,ty,e) -> Function (id,ty,desugar e) 
      | Pair(a,b) -> Pair( (desugar a), (desugar b))
      | Apply (a,b) -> Apply ((desugar a),(desugar b)) 
      | Fst a -> Fst (desugar a)
      | Snd a -> Snd (desugar a) in
     
    (* miniml -> interm + var list side effect *)
    let rec mcompile varlist program = 
      let is_intop str = 
        let findstr str2 = match (String.compare str str2) with 0 -> true | _ -> false in
        (List.exists findstr int_op) in
      let rec convert : MiniML.term -> tempc = function 
        | Longident lpath -> let cpath = (convert_path lpath) in 
           (try (match (lookup_path env cpath) with
              | Static spath -> ToCast (VALUE,(ToCall ((CVar spath),[]))) 
              | Dynamic (nn, Some ls) -> let path = CString (make_path ls)
                and size = CInt (List.length ls) in
                  (ToCall ((constc PATHV),[ (constv MOD); path ; size])) 
              | _ -> raise (Cannot_compile "Did not retrieve path from lookup"))
           with _ -> (Get ((constv ENV),(CString (make_entrypoint cpath)))))
        | Constant x -> ToInt (CInt x)
        | Boolean x -> ToBoolean (CInt (match x with | true -> 1 | _ -> 0))
        | If (a,b,c) -> ToQuestion ((ToBValue (convert a)),(convert b),(convert c))
        | Pair(a,b) -> ToPair ((convert  a), (convert b))
        | Apply (l,r) -> let tmp = new_var() in 
           varlist := (CVar tmp) :: !varlist;
           let tcv = (CVar tmp) in
           let args = [(ToMod tcv);(ToEnv tcv); (convert r)] in
           ToComma(Assign( tcv, (convert l)),ToCast (VALUE,(ToCall ((ToLambda tcv),args))))
        | Function(id,ty,e) -> let idn = (Ident.name id) in
          let lamname = (new_func (make_ptr path))  in
          (*let convert_ty = (parse_type ty) in*)
          (makef lamname idn e);
          ToClosure((constv MOD),(constv ENV),(*convert_ty,*)(CVar lamname)) 
        | Prim (s,ls) when (List.length ls) == 2 -> let left = ToIValue(convert (List.hd ls)) in 
          let right = ToIValue((convert (List.hd (List.tl ls)))) in
          let operation = ToOper(s,left,right) in
            if (is_intop s) then (ToInt operation) else (ToBoolean operation)
        | Prim (s,ls) -> raise (Cannot_compile "multi parameter int/bool operands not supported")
        | Fst a -> (ToLeft (convert a))
        | Snd a -> (ToRight (convert a))
        | _ -> raise (Cannot_compile "Failed to wipe out the lets") in
      (convert program)

      (* create a lambda function *)
      and makef name id e = let vlist = ref [] in
        let compiled = mcompile vlist e in
        let ptrarg = CVar (new_ptr()) in
        let ptrstr = CVar (new_ptr()) in
        let malla = MALLOC (VALUE,ptrarg,(Sizeof VALUE)) in
        let assarg = Assign((Ptr ptrarg),(constv ARG)) in
        let asstr = ToStatic((constd CHAR),Assign((Ptr ptrstr),(CString id))) in
        let insert = (Insert((constv ENV),ptrstr,ptrarg)) in
        let compttr = Compttr (name,(!vlist,[],compiled),[asstr; malla ; assarg; insert]) in
        funclist := compttr :: !funclist in

    (* toplevel *)
    let var_list = ref [] in
    let computation = (mcompile var_list (desugar program)) in 
    (!funclist,(!var_list,[],computation))

end



