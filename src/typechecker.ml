(*
 * =====================================================================================
 *
 *     Filename:  typechecker.ml
 *
 *  Description:  Typechecker for MiniML ML
 *
 *     Author:  Adriaan Larmuseau, ajhl
 *    Company:  Uppsala IT
 *
 * =====================================================================================
 *)

(* Requires the MiniML specification and Leroy Modular Modules *)
open Mini
open Modules


(* Exceptions *) 
type typefail = Expansion | Unification | Random of string
exception Cannot_TypeCheck of typefail

let string_typefail = function
  | Expansion -> "Could not expand!"
  | Unification -> "Could not unify!"
  | Random s -> s


(*-----------------------------------------------------------------------------
 *  MiniML ML typing
 *-----------------------------------------------------------------------------*)

module MiniMLTyping  =
struct
  module Core = MiniML 
  module Env = MiniMLEnv
  open MiniML 


 (*-----------------------------------------------------------------------------
  *  Global Variables 
  *-----------------------------------------------------------------------------*)

  let current_level = ref 0
  let begin_def() = incr current_level
  let end_def() = decr current_level


 (*-----------------------------------------------------------------------------
  *  Helper Funcions
  *-----------------------------------------------------------------------------*)

  (* produce an empty variable *) 
  let newvar() = {repres = None; level = !current_level}
  let unknown() = Var(newvar())

  (* Replace variable type with argument *)
  let rec subst_vars subst ty =
    match typerepr ty with
    Var var as tyvar ->
      begin try List.assq var subst with Not_found -> tyvar end
    | Typeconstr(p, tl) -> Typeconstr(p, List.map (subst_vars subst) tl)
    | LambdaType(tly,tl) -> LambdaType(tly,List.map (subst_vars subst) tl)

  (* Check if one of the members is to be ignored *)
  let is_ignore t1 t2 = match (t1,t2) with
    | (TIgnore,_) -> true
    | (_,TIgnore) -> true
    | _ -> false

  (* unify lambda types *)
  let compare_lty t1 t2 =  if (is_ignore t1 t2) then true
    else t1 = t2

  (* TODO what does this do *)
  let instance vty = match vty.quantif with [] -> vty.body
    | vars -> subst_vars (List.map (fun v -> (v, unknown())) vars) vty.body

  let expand_manifest env path args =
    match Env.find_type path env with
    {MiniMLMod.manifest = None} ->
      raise (Cannot_TypeCheck Expansion)
    | {MiniMLMod.manifest = Some def} ->
      subst_vars (List.combine def.params args) def.defbody

  (* Expand abbreviations in ty1 and ty2 until their top constructor match *)
  let rec scrape_types env ty1 ty2 =
    let repr1 = typerepr ty1 
    and repr2 = typerepr ty2 in
    match (repr1, repr2) with 
    (Typeconstr(path1, args1), Typeconstr(path2, args2)) ->
      if path_equal path1 path2 then
      begin
      (repr1, repr2)
    end
      else begin
      try
        scrape_types env (expand_manifest env path1 args1) repr2
      with Cannot_TypeCheck Expansion ->
        try
        scrape_types env repr1 (expand_manifest env path2 args2)
        with Cannot_TypeCheck Expansion ->
        (repr1, repr2)
      end
    | (Typeconstr(path, args), _) ->
      begin try
      scrape_types env (expand_manifest env path args) repr2
      with Cannot_TypeCheck Expansion -> (repr1, repr2)
      end
    | (_, Typeconstr(path, args)) ->
      begin try
      scrape_types env repr1 (expand_manifest env path args)
      with Cannot_TypeCheck Expansion ->
      (repr1, repr2)
      end
    | (_, _) -> (repr1, repr2)

  let rec occur_check var ty =
    match typerepr ty with
    Var var' -> if var == var' then error "cycle in unification"
    | Typeconstr(p, tl) -> List.iter (occur_check var) tl
    | LambdaType(_,tl) -> List.iter (occur_check var) tl

  let rec update_levels level_max ty =
    match typerepr ty with
    Var v -> if v.level > level_max then v.level <- level_max
    | Typeconstr(p, tl) -> List.iter (update_levels level_max) tl
    | LambdaType(_, tl) -> List.iter (update_levels level_max) tl

  let rec check_simple_type env params ty =
    match typerepr ty with
    Var v ->
      if not (List.memq v params) then error "free type variable"
    | Typeconstr(path, tl) ->
      let arity = (Env.find_type path env).MiniMLMod.kind.arity in
      if List.length tl <> arity then error "arity error";
      List.iter (check_simple_type env params) tl
    | LambdaType(_,tl) -> (List.iter (check_simple_type env params) tl)

  let kind_deftype env def =
    check_simple_type env def.params def.defbody;
    {arity = List.length def.params}

  let check_valtype env vty =
    check_simple_type env vty.quantif vty.body

  let check_kind env kind = ()


  (* 
   * ===  FUNCTION  ======================================================================
   *     Name:  generalize
   *  Description: Why does this generalize for higher level variables shouldn't it be lower ?
   * =====================================================================================
   *)
  let generalize ty =
    let rec gen_vars vars ty = match typerepr ty with
      Var v -> if v.level > !current_level && not (List.memq v vars)
        then v :: vars
        else vars
      | Typeconstr(path, tl) -> List.fold_left gen_vars vars tl 
      | LambdaType(_,tl) -> List.fold_left gen_vars vars tl 
    in 
    { quantif = gen_vars [] ty; body = ty }

  (* 
   * ===  FUNCTION  ======================================================================
   *     Name:  type_term
   *  Description: types the internal lambda calculus terms
   * =====================================================================================
   *)
  let type_term env term =

    (* helper *)
    let trivial_scheme ty = { quantif = []; body = ty } in

    (* unify types *)
    let rec unify env t1 t2 =
      
      (* top level *)
      match scrape_types env t1 t2 with
        (r1, r2) when r1 == r2 -> ()
      | (Var v, r2) -> occur_check v r2;
         update_levels v.level r2;
         v.repres <- Some r2
      | (r1, Var v) -> occur_check v r1;
         update_levels v.level r1;
         v.repres <- Some r1
      | (Typeconstr(path1, args1), Typeconstr(path2, args2)) when path1 = path2 ->
         List.iter2 (unify env) args1 args2
      | (LambdaType (lty1,ls1), LambdaType (lty2,ls2)) when (compare_lty lty1 lty2) ->
         if (is_ignore lty1 lty2) then ()
         else (List.iter2 (unify env) ls1 ls2)
      | (_, _) -> raise (Cannot_TypeCheck Unification)
    in

    (* infer the Lambda calc type *)
    let rec infer_type env = function
        Constant _ -> MiniML.int_type
      | Boolean _ -> MiniML.bool_type
      | Longident path -> instance (Env.find_value path env) 
      | Function(param,ty,body) -> let quantifier = (trivial_scheme ty) in
        let type_body = infer_type (Env.add_value param quantifier env) body in
        MiniML.arrow_type ty type_body
      | Apply(funct, arg) -> let type_funct = infer_type env funct in
        let type_arg = infer_type env arg in
        let type_result = unknown() in
        unify env type_funct (MiniML.arrow_type type_arg type_result);
        type_result
      | Let(ident, arg, body) -> begin_def();
        let type_arg = infer_type env arg in
        end_def();
        let _ = (Env.add_value ident (generalize type_arg) env) in
        let tt = infer_type (Env.add_value ident (generalize type_arg) env) body in tt
      | If (t1,t2,t3) -> let t1_type = infer_type env t1 
        and t2_type = infer_type env t2 
        and t3_type = infer_type env t3 in
        unify env t1_type MiniML.bool_type;
        unify env t2_type t3_type;
        t3_type
      | Pair (t1,t2) -> let t1_type = infer_type env t1 
        and t2_type = infer_type env t2 in 
        (pair_type t1_type t2_type)
      | Fst t1 -> let t1_type = infer_type env t1 in
        let result = unknown() in
        let irrel = unknown() in
        unify env t1_type (MiniML.pair_type result irrel);
        result
      | Snd t1 -> let t1_type = infer_type env t1 in
        let result = unknown() in
        let irrel = unknown() in
        unify env t1_type (MiniML.pair_type irrel result);
        result
      | Prim (str,ls) -> 
        let t1_type = infer_type env (List.hd ls) 
        and t2_type = infer_type env (List.hd (List.tl ls))  in
        match str with
        | "+" | "*" | "-" | "/" -> 
          unify env t2_type MiniML.int_type;
          unify env t1_type MiniML.int_type;
          MiniML.int_type
        | "==" | "<=" | ">=" | ">" | "<" -> 
          unify env t2_type MiniML.int_type;
          unify env t1_type MiniML.int_type;
          MiniML.bool_type
        | _ -> raise (Cannot_TypeCheck (Random "Couldn't match Prim string"))
    in

    (* top level *)
    begin_def(); 
    let ty = infer_type env term in
    end_def();
    generalize ty (* TODO remove ? *)

  (* 
   * ===  FUNCTION  ======================================================================
   *     Name:  valtype_match
   *  Description: match value types
   * =====================================================================================
   *)
  let valtype_match env vty1 vty2 =
    let rec filter ty1 ty2 =
    match scrape_types env ty1 ty2 with
      (Var v, ty2) ->
      if List.memq v vty2.quantif
      then false
      else (v.repres <- Some ty2; true)
    | (Typeconstr(path1, tl1), Typeconstr(path2, tl2)) ->
      path1 = path2 && List.for_all2 filter tl1 tl2
    | (LambdaType (tly1,tl1), LambdaType(tly2,tl2)) ->
        (compare_lty tly1 tly2) && List.for_all2 filter tl1 tl2
    | (_, _) -> false in
    filter (instance vty1) vty2.body

  let deftype_equiv env kind def1 def2 =
    let rec equiv ty1 ty2 =
    match scrape_types env ty1 ty2 with
      (Var v1, Var v2) -> v1 == v2
    | (Typeconstr(path1, args1), Typeconstr(path2, args2)) ->
      path1 = path2 && List.for_all2 equiv args1 args2
    | (LambdaType (lty1,tl1) , LambdaType (lty2, tl2)) ->
      (compare_lty lty1 lty2) && (List.for_all2 equiv tl1 tl2)
    | (_, _) -> false in
    let subst =
    List.map2 (fun v1 v2 -> (v2, Var v1)) def1.params def2.params in
    equiv def1.defbody (subst_vars subst def2.defbody)

  let kind_match env kind1 kind2 =
    kind1.arity = kind2.arity

  let deftype_of_path path kind =
    let rec make_params n =
    if n <= 0 then [] else newvar() :: make_params (n-1) in
    let params = make_params kind.arity in
    { params = params;
    defbody = Typeconstr(path, List.map (fun v -> Var v) params) }

  (* Elimination of dependencies on a given module identifier
     by repeated expansion of type paths rooted at that identifier.
     Those functions are used only with the relaxed typing rule
     for functor applications described in section 5.5 and implemented
     in the file modules.ml.extended *)

  let rec is_rooted_at id = function
    Pident id' -> Ident.equal id id'
    | Pdot(p, s) -> is_rooted_at id p

  let rec nondep_type env id ty =
    match typerepr ty with
    Var v as tvar -> tvar
    | LambdaType(tly,ls) -> LambdaType(tly, (List.map (nondep_type env id) ls))
    | Typeconstr(path, args) ->
      if is_rooted_at id path then begin
      try
        nondep_type env id (expand_manifest env path args)
      with Cannot_TypeCheck Expansion ->
        raise Not_found
      end else
      Typeconstr(path, List.map (nondep_type env id) args)
        

  let nondep_valtype env id vty =
    { quantif = vty.quantif; body = nondep_type env id vty.body }
  let nondep_deftype env id def =
    { params = def.params; defbody = nondep_type env id def.defbody }
  let nondep_kind env id kind =
    kind
end

(*-----------------------------------------------------------------------------
 *  Apply the Modular Module system to the typing
 *-----------------------------------------------------------------------------*)
module MiniMLModTyping = Mod_typing(MiniMLMod)(MiniMLEnv)(MiniMLTyping)(Printer.Pretty) 

