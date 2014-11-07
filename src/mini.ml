(*
 * =====================================================================================
 *
 *      Filename:  miniml.ml
 *
 *   Description:  MiniML term and type specification (Modules not included)
 *  Extension of:  Xavier Leroy's modular Modules implementation
 *
 *        Author:  Adriaan Larmuseau, ajhl
 *       Company:  Uppsala IT
 *
 * =====================================================================================
 *)

open Modules

module MiniML =
struct

 (*-----------------------------------------------------------------------------
  *  MiniML's non module term's and expressions
  *-----------------------------------------------------------------------------*)
  type term =
      Constant of int            
    | Boolean of bool            
    | Longident of path           
    | Pair of term * term
    | Function of Ident.t * simple_type * term 
    | Apply of term * term            
    | If of term * term * term
    | Let of Ident.t * term * term    
    | Prim of string * (term list)
    | Fst of term
    | Snd of term


 (*-----------------------------------------------------------------------------
  *  MiniML's type system
  *-----------------------------------------------------------------------------*)
  and simple_type = 
      Var of type_variable 
    | LambdaType of lambda_type * simple_type list
    | Typeconstr of path * simple_type list  

  and type_variable =
    { mutable repres: simple_type option; mutable level: int } (* what's level for ?*)

  and lambda_type = TBool | TInt | TArrow | TPair | TIgnore

  type val_type =
    { quantif: type_variable list;     
    body: simple_type }         

  type def_type =
    { params: type_variable list;    
    defbody: simple_type }      
  type kind = { arity: int } (* TODO remove kinds ? *)

  (* Type constructors *)
  let arrow_type t1 t2 = LambdaType(TArrow,[t1;t2])
  let bool_type = LambdaType(TBool, [])
  let int_type = LambdaType(TInt, [])
  let pair_type t1 t2 = LambdaType(TPair,[t1;t2])
  let ignore_type = LambdaType(TIgnore,[])

  (* Subtype *)
  let rec subst_type subst = function
    Var {repres = None} as ty -> ty
    | Var {repres = Some ty} -> subst_type subst ty
    | Typeconstr(p, tl) ->
      Typeconstr(Subst.path p subst, List.map (subst_type subst) tl)

  let subst_valtype vty subst =
    { quantif = vty.quantif;
    body = subst_type subst vty.body }

  let subst_deftype def subst =
    { params = def.params;
    defbody = subst_type subst def.defbody }

  let subst_kind kind subst = kind

end

(*-----------------------------------------------------------------------------
 *  Apply the Modular Module system to the syntax 
 *-----------------------------------------------------------------------------*)
module MiniMLMod = Mod_syntax(MiniML)
module MiniMLEnv = Env(MiniMLMod)


