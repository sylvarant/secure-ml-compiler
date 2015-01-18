(*
 * =====================================================================================
 *
 *      Filename:  miniml.ml
 *
 *   Description:  MiniML term and type scoping 
 *  Extension of:  Xavier Leroy's modular Modules implementation
 *
 *        Author:  Adriaan Larmuseau, ajhl
 *       Company:  Uppsala IT
 *
 * =====================================================================================
 *)

open Mini
open Modules

module MiniMLScoping =
struct
  module Core = MiniML (* required by sig *) 
  open MiniML 

  (* 
   * ===  FUNCTION  ======================================================================
   *     Name:  scope_simple_type
   *  Description:  Allows the modular module system scoper to figure out the id's 
   * =====================================================================================
   *)
  let rec scope_simple_type sc = function
      Var v -> Var v
    | LambdaType(lty,ls) -> let sc_ls = (List.map (scope_simple_type sc) ls) in
        LambdaType(lty,sc_ls) 
    | Typeconstr(path,ls) -> let sc_path = (Scope.type_path path sc) in
        let sc_ls = (List.map (scope_simple_type sc) ls) in
        Typeconstr(sc_path,sc_ls)

  (* 
   * ===  FUNCTION  ======================================================================
   *     Name:  scope_term
   *  Description:  Allows the modular module system scoper to figure out the id's 
   * =====================================================================================
   *)
  let rec scope_term sc = function
   | Longident path -> Longident(Scope.value_path path sc)
   | Function(id,ty,body) -> let scoped_ty = (scope_simple_type sc ty) in
       let scoped_body = (scope_term (Scope.enter_value id sc) body) in
       Function(id,scoped_ty,scoped_body)
   | Apply(t1, t2) -> Apply(scope_term sc t1, scope_term sc t2)
   | Let(id, t1, t2) -> let scoped_t2 = (scope_term (Scope.enter_value id sc) t2) in
       Let(id, scope_term sc t1,scoped_t2)
   | If (t1,t2,t3) -> If(scope_term sc t1,scope_term sc t2,scope_term sc t3)
   | Prim (c,ls) -> Prim(c,(List.map  (fun x -> (scope_term sc x)) ls))
   | Fst t1 ->  Fst (scope_term sc t1)
   | Snd t1 ->  Snd (scope_term sc t1)
   | Pair (t1,t2) -> Pair ((scope_term sc t1), (scope_term sc t2))
   | Sequence (t1,t2) -> Sequence ((scope_term sc t1),(scope_term sc t2))
   | _ as t -> t


  (* value types , definitions and kinds *)
  let scope_valtype sc vty =
    { quantif = vty.quantif; body = scope_simple_type sc vty.body }

  let scope_deftype sc def =
    { params = def.params; defbody = scope_simple_type sc def.defbody }

  let scope_kind sc kind = kind

end

(*-----------------------------------------------------------------------------
 *  Apply the Modular Module system to the scoping
 *-----------------------------------------------------------------------------*)
module MiniMLModScoping = ModScoping(MiniMLMod)(MiniMLScoping)


