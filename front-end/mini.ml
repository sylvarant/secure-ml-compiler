(*
 * =====================================================================================
 *
 *       Filename:  miniml.ml
 *
 *    Description:  MiniML specification
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 *)

(* Requires the Leroy framework for modular modules *)
open Modules

module MiniMLDebug : sig
    
    val debug : string -> unit

end =
struct

    let debug str = Printf.eprintf "%s" str
    
end

(*-----------------------------------------------------------------------------
 *  MiniML version of ML
 *-----------------------------------------------------------------------------*)
module MiniML =
struct

    (* language AST *) (* TODO add lists and pairs *)
    type term =
        Constant of int                        
      | Boolean of bool                      
      | Longident of path                     
      | Function of Ident.t * term (* TODO type annotate function var's ? *)
      | Apply of term * term               
      | If of term * term * term
      | Let of Ident.t * term * term      
      | Prim of string * (term list)

    (* Type system *) 
    and simple_type =
        Var of type_variable                   
      | Typeconstr of path * simple_type list  

    and type_variable =
      { mutable repres: simple_type option;   
              mutable level: int }        


    type val_type =
      { quantif: type_variable list;         
        body: simple_type }                 

    type def_type =
      { params: type_variable list;        
        defbody: simple_type }            
    type kind = { arity: int }

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


    (*------------------------ Base types  ------------------------*)

    (* Arrow type *)
    let ident_arrow = Ident.create "->"
    let path_arrow = Pident ident_arrow
    let arrow_type t1 t2 = Typeconstr(path_arrow, [t1;t2])

    (* Integer type *)
    let ident_int = Ident.create "int"
    let path_int = Pident ident_int
    let int_type = Typeconstr(path_int, [])


    (* Boolean type *)
    let ident_bool = Ident.create "bool"
    let path_bool = Pident ident_bool
    let bool_type = Typeconstr(path_bool, [])

    (* Star type *)
    let ident_star = Ident.create "*"
    let path_star = Pident ident_star

    

end

(*-----------------------------------------------------------------------------
 *  Apply the Modular Module system to the syntax 
 *-----------------------------------------------------------------------------*)
module MiniMLMod = Mod_syntax(MiniML)
module MiniMLEnv = Env(MiniMLMod)


