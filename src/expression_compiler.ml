(*
 * =====================================================================================
 *
 *     Filename:  expression_compiler.ml
 *
 *  Description:  Compile the AST into C
 *
 *     Author:  MYSTERY MAN, 
 *    Company:  SOMEWHERE IT
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
      | Longident _ as t -> t
      | If (a,b,c) -> If ((desugar a),(desugar b),(desugar c))
      | Prim (op,ls) -> let nls = (List.map desugar ls) in Prim(op,nls)
      | Function(id,ty,e) -> Function (id,ty,desugar e) 
      | Pair(a,b) -> Pair( (desugar a), (desugar b))
      | Apply (a,b) -> Apply ((desugar a),(desugar b)) 
      | Fst a -> Fst (desugar a)
      | Snd a -> Snd (desugar a) 
      | Sequence (a,b) -> Sequence ((desugar a),(desugar b))
      | Ref a -> Ref (desugar a)
      | Deref a -> Deref (desugar a)
      | Assign (a,b) -> Assign ((desugar a),(desugar b))
      | Letrec (id,ty,a,b) -> desugar (Let(id,Fix(Function(id,ty,a)),b))
      | _ as t -> t
    in
     
    (* miniml -> interm + var list side effect *)
    let rec mcompile varlist program = 
      let is_intop str = 
        let findstr str2 = match (String.compare str str2) with 0 -> true | _ -> false in
        (List.exists findstr int_op) in
      let rec convert : MiniML.term -> tempc = function 
        | Longident lpath -> let cpath = (convert_path lpath) in 
           (*Printf.eprintf "Looking for %s" (String.concat "_" cpath);*)
           (try (match (lookup_path env cpath) with
              | Static spath -> ToCast (VALUE,(ToCall ((CVar spath),[(constv MOD)]))) 
              | Dynamic (nn, Some ls) -> let path = CString (make_path ls) in
                  (ToCall ((constc PATHV),[ (constv MOD); path])) 
              | _ -> raise (Cannot_compile "Did not retrieve path from lookup"))
           with _ -> let Some eptr = (make_entrypoint cpath) in (Get ((constv ENV),(CString eptr))))
        | Constant x -> ToInt (CInt x)
        | Unit -> ToUnit
        | Boolean x -> (match x with | true -> ToTrue | _ -> ToFalse)
        | If (a,b,c) -> ToQuestion ((ToBValue (convert a)),(convert b),(convert c))
        | Pair(a,b) -> ToPair ((convert a), (convert b))
        | Apply (l,r) -> let tmp = new_var() in 
           varlist := (CVar tmp) :: !varlist;
           let tcv = (CVar tmp) in
           let args = [(ToMod tcv);(ToEnv tcv); (convert r)] in
           ToComma(Assign( tcv, (convert l)),ToCast (VALUE,(ToCall ((ToLambda tcv),args))))
        | Sequence (a,b) -> ToComma((convert a),(convert b))
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
        | Ref a -> (ToLocation (convert a))
        | Deref a -> (ToDeref (convert a))
        | Exit a -> ToExit (convert a)
        | Fix a -> ToFix (convert a)
        | Assign (a,b) -> (ToAssign ((convert a),(convert b)))
        | _ -> raise (Cannot_compile "Failed to wipe out the lets") in
      (convert program)

      (* create a lambda function *)
      and makef name id e = let vlist = ref [] in
        let compiled = mcompile vlist e in
        let ptrarg = CVar (new_ptr()) in
        let ptrstr = CVar (new_ptr()) in
        let malla = MALLOC (VALUE,ptrarg,(Sizeof VALUE)) in
        let assarg = Assign((Ptr ptrarg),(constv ARG)) in
        let asstr = ToStatic((constd CHAR),Assign(ptrstr,(CString id))) in
        let insert = (Insert((constv ENV),ptrstr,ptrarg)) in
        let compttr = Compttr {name = name; comp = (!vlist,[],compiled); setup = [asstr; malla ; assarg; insert]} in
        funclist := compttr :: !funclist in

    (* toplevel *)
    let var_list = ref [] in
    let computation = (mcompile var_list (desugar program)) in 
    (!funclist,(!var_list,[],computation))

end



