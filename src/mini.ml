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
    | Sequence of term * term
    | Ref of term
    | Deref of term
    | Assign of term * term
    | Unit


 (*-----------------------------------------------------------------------------
  *  MiniML's type system
  *-----------------------------------------------------------------------------*)
  and simple_type = 
      Var of type_variable 
    | LambdaType of lambda_type * simple_type list
    | Typeconstr of path * simple_type list  

  and type_variable =
    { mutable repres: simple_type option; mutable level: int } (* what's level for ?*)

  and lambda_type = TBool | TInt | TArrow | TPair | TIgnore | TUnit | TRef

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
  let unit_type = LambdaType(TUnit,[])
  let ref_type t1 = LambdaType(TRef,[t1])
  let pair_type t1 t2 = LambdaType(TPair,[t1;t2])
  let ignore_type = LambdaType(TIgnore,[])

  (* Substitution *)
  let rec subst_type subst = function
    Var {repres = None} as ty -> ty
    | Var {repres = Some ty} -> subst_type subst ty
    | Typeconstr(p, tl) -> Typeconstr(Subst.path p subst, List.map (subst_type subst) tl)
    | LambdaType(lty,tl) -> LambdaType( lty ,List.map (subst_type subst) tl)

  let subst_valtype vty subst =
    { quantif = vty.quantif;
    body = subst_type subst vty.body }

  let subst_deftype def subst =
    { params = def.params;
    defbody = subst_type subst def.defbody }

  let subst_kind kind subst = kind

  (* Helpers *)
  let rec typerepr = function 
      Var({repres = Some ty} as var) -> let r = typerepr ty in 
        var.repres <- Some r; r
    | ty -> ty

  let path_to_simple path kind =
    let rec make_params n =
    if n <= 0 then [] else {repres = None; level = 0} :: make_params (n-1) in
    let params = make_params kind.arity in
    { params = params;
    defbody = Typeconstr(path, List.map (fun v -> Var v) params) }


end

(*-----------------------------------------------------------------------------
 *  Apply the Modular Module system to the syntax 
 *-----------------------------------------------------------------------------*)
module MiniMLMod = Mod_syntax(MiniML)
module MiniMLEnv = Env(MiniMLMod)


