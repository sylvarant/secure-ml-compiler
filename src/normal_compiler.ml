(*
 * =====================================================================================
 *
 *     Filename:  normal_compiler.ml
 *
 *  Description:  Compile the AST into C in a non secure way
 *
 *     Author:  Adriaan, 
 *    Company:  Uppsala IT IT
 *
 * =====================================================================================
 *)

open Mini
open Modules 
open Modules_compiler
open Expression_compiler

(*-----------------------------------------------------------------------------
 * Setup Module Compiler 
 *-----------------------------------------------------------------------------*)
module MiniModComp = ModComp(MiniMLComp)


(* ===  MODULE  ======================================================================
 *         Name:  NormCompiler
 *  Description:  The normal compiler
 * =====================================================================================
 *)
module NormCompiler : CCOMPILER =
struct
  
  module MC = MiniModComp
  open MiniML 
  open MiniMLMod
  open MC.MOmega
  open MiniMLComp.Intermediary


 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)

  type compilertype = MiniMLMod.mod_type -> MiniMLMod.mod_term -> string -> (string * string)
  exception Cannot_Norm_Compile of string


 (*-----------------------------------------------------------------------------
  *  Helper Funcions
  *-----------------------------------------------------------------------------*)

  (* convert list of compiler redices into strings of function definitions *)
  let mapfd ls = (List.map printf 
    (List.map (fun x -> (MC.Low.funcdef true x)) ls))


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  compile
  *  Description:  converts the toplevel into a tuple of 2 strings for object & header
  * =====================================================================================
  *)
  let compile mty program headerf =

    (* Top Level *)
    let (lambdas,omega) = (MC.High.compile program) in
    let (gettrs,strcts,fctrs) = (MC.High.extract_red omega) in

    (* is a real entrypoint *)
    let convert_entry = function
      | Strct (t,a,b,c,d,e,ls) -> let nls = (List.map (function _ -> None) ls) in 
          Strct (t,a,b,c,d,e,nls)
      | _ -> raise (Cannot_Norm_Compile "Only strct can deal with entry conversion")
    in

    let global_strct = function
      | Strct (_,n,pth,_,_,_,_) -> let name = (make_str (n::pth)) in
        (printl ENTRYPOINT)^ " " ^(printd MODULE)^" "^name^";" 
    in 

    let global_var = [Assign(CVar "unsigned int LOADED",CInt 0)] in

    (* replace entrypoints TODO this could be nicer *)
    let nstrcts = (List.map convert_entry strcts) in

    (* build the header *)
    let hedh = header  (List.map printc [(consth ENTRY); (consth MINI)]) in
    let dec_ls = (separate "Declarations" (mapfd (gettrs@fctrs))) @ (List.map global_strct nstrcts) in
    let headerfile = (String.concat "\n" ("#ifndef LANREN\n#define LANREN"::(hedh @ dec_ls)@["#endif"])) ^ "\n"  in

    (* build the object file *)
    let glb_ls = (separate "Globals" (List.map (fun x -> x^";") (List.map printc global_var)))
    and dec_ls = (separate "Declarations" (mapfd (gettrs@fctrs)))
    and pb_ls = (separate "Static Structures" (MC.Low.structure nstrcts))
    and pl_ls = (separate "Closures" (MC.Low.lambda (List.rev lambdas)))
    and pv_ls = (separate "Values" (MC.Low.getter (List.rev gettrs)))
    and fc_ls = (separate "Functors" (MC.Low.lambdaf (List.rev fctrs)))
    and ld_ls = (separate "Load" [(MC.Low.load nstrcts)]) 
    and objh =  header (List.map printc [(Include headerf)]) in
    let bigls = (objh @ glb_ls @ dec_ls @ pb_ls @ pl_ls @ pv_ls @ fc_ls @ ld_ls @ footer) in
    let objectfile = ((String.concat "\n" bigls) ^ "\n")
    in

    (* the two files *)
    (objectfile,headerfile)
  
end

