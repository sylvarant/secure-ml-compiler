(*
 * =====================================================================================
 *
 *     Filename:  secure_compiler.ml
 *
 *  Description:  Compile the AST into C in a fully abstract way
 *
 *     Author:  Adriaan Larmuseau, ajhl
 *    Company:  Uppsala IT
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
 *         Name:  SecCompiler
 *  Description:  The secure compiler
 * =====================================================================================
 *)
module SecCompiler : CCOMPILER =
struct
  
  module MC = MiniModComp
  open MiniML 
  open MiniMLMod
  open MC.MOmega
  open MiniMLComp.Intermediary


 (*-----------------------------------------------------------------------------
  *  Exceptions
  *-----------------------------------------------------------------------------*)
  exception Found of string list


 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)

  type compilertype = MiniMLMod.mod_type -> MiniMLMod.mod_term -> string -> (string * string)

  type typetrawl = Fail | SimpleType of simple_type | Modtype of mod_type 
                 | ManifestType of def_type option

  type assoc = Call of val_type * string * string list
             | Share of MiniMLMod.mod_type * string * string list * string list
             | Fu of MiniMLMod.mod_type * string * string * string list * string list
 

 (*-----------------------------------------------------------------------------
  *  Helper Funcions
  *-----------------------------------------------------------------------------*)

  (* sort signature components *)
  let sort_sigs sls =
    let cmp_sig a b = match(a,b) with
      | (Value_sig (id1,_) , Value_sig(id2,_)) -> (String.compare (Ident.name id1) (Ident.name id2))
      | (Module_sig (id1,_), Module_sig(id2,_)) -> (String.compare (Ident.name id1) (Ident.name id2))
      | (Type_sig (id1,_), Type_sig(id2,_)) -> (String.compare (Ident.name id1) (Ident.name id2))
      | (Type_sig _ , _) -> -1  
      | (Module_sig _, _) -> 1
      | (Value_sig _, Type_sig _ ) -> 1
      | (Value_sig _ , Module_sig _ ) -> -1
    in
    (List.sort cmp_sig sls)

  (* sort bindings *)
  let sort_bindings bls = 
    let cmp_binding a b = match (a,b) with
      | (BVal (name1,_,_) , BVal(name2,_,_)) -> (String.compare name1 name2)
      | (BMod (name1,_) , BMod(name2,_)) -> (String.compare name1 name2)
      | (BVal _, _) -> -1
      | (BMod _, _) -> 1
    in
    (List.sort cmp_binding bls)

  (* sort compiler redices *)
  let sort_compred cls =
    let cmp_red a b = match (a,b) with
      | (Gettr(str,_,_,_),Gettr(str2,_,_,_)) -> (String.compare str str2)
      | (Strct pth , Strct pth2) -> (String.compare (make_ptr pth) (make_ptr pth2))
      | (Fctr (str,_),Fctr (str2,_)) -> (String.compare str str2)
      | (Compttr (str,_,_),Compttr (str2,_,_)) -> (String.compare str str2)
      | (Gettr _, _)     -> 1
      | (_, Gettr _)     -> -1
      | (Compttr _, _)   -> -1
      | (_, Compttr _)   -> 1
      | (Strct _,Fctr _) -> 1
      | (Fctr _,Strct _) -> -1
    in
    (List.sort cmp_red cls)

  (* sort assoc *)
  let sort_assocs als =
    let cmp_ass a b = match (a,b) with 
      | (Call (_,n,pth),Call (_,n2,pth2)) -> (String.compare (make_ptr (n::pth)) (make_ptr (n2::pth2))) 
      | (Share (_,n,_,pth),Share (_,n2,_,pth2)) -> (String.compare (make_ptr (n::pth)) (make_ptr (n2::pth2)))
      | (Fu (_,n,_,_,pth),Fu (_,n2,_,_,pth2)) -> (String.compare (make_ptr (n::pth)) (make_ptr (n2::pth2)))
      | (Call _, _)  -> 1
      | (Fu _, _) -> -1
      | (Share _,Call _) -> -1
      | (Share _,Fu _) -> 1
    in
    (List.sort cmp_ass als)

  (* sort per path *)
  let sort_assocspth als =
    let ccmp a b =  
      if ((List.length a) == (List.length b))
      then (String.compare (make_ptr a) (make_ptr b))
      else (Pervasives.compare (List.length a) (List.length b))
    in
    let cmp_ass a b = match (a,b) with
      | (Call (_,_,p), Call (_,_,p2)) -> ccmp p p2
      | (Share (_,_,_,p), Share (_,_,_,p2)) -> ccmp p p2
      | (Fu (_,_,_,_,p), Fu (_,_,_,_,p2)) -> ccmp p p2
      | (Call (_,_,p), Share (_,_,_,p2)) -> ccmp p p2
      | (Call (_,_,p), Fu (_,_,_,_,p2)) -> ccmp p p2
      | (Share (_,_,_,p), Call(_,_,p2)) -> ccmp p p2
      | (Share (_,_,_,p), Fu (_,_,_,_,p2)) -> ccmp p p2
      | (Fu (_,_,_,_,p), Share (_,_,_,p2)) -> ccmp p p2
      | (Fu (_,_,_,_,p), Call (_,_,p2)) -> ccmp p p2
    in
    (List.sort cmp_ass als)

  (* split associations into a sequence of path related assocs *)
  let split_assocpth als =
    let rec filtr (tar : string list) (curr : assoc list) : assoc list -> assoc list list = function [] -> curr :: []
      | l :: ls -> match l with
        | Call (_,_,p) as c when (make_ptr p) = (make_ptr tar) ->
            (filtr tar (c::curr) ls)
        | Call (_,_,p) as c -> curr :: (filtr p [c] ls)
        | Share (_,str,_,path) as s when (make_ptr path) = (make_ptr tar) ->
            (filtr tar (s::curr) ls)
        | Share (_,str,p,path) as s -> curr :: (filtr path [s] ls)
        | Fu (_,str,_,_,path) as f when (make_ptr path) = (make_ptr tar) ->
            (filtr tar (f::curr) ls)
        | Fu (_,str,_,_,path) as f -> curr :: (filtr path [f] ls)
    in
    (List.rev (filtr [] [] (sort_assocspth als)))
          
  (* compare statics *)
  let cmp_stat a b = match a with 
    ToStatic (_,Assign(_,CString id)) -> (match b with 
      ToStatic(_,Assign(_,CString id2)) -> (String.compare id id2)
      | _ -> -1) 
    | _ -> -1 


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name: look_up_type_path
  *  Description: find a path in the modules type decl
  * =====================================================================================
  *)
  let rec look_up_type_path ty path = 
    (* find a string in a mod_type *)
    let rec find str mty = 
      let rec find_decl = function [] -> Fail
        | x::xs -> match x with
          | Value_sig(id, vty) when (Ident.name id) = str -> SimpleType vty.body
          | Type_sig(id, decl) when (Ident.name id) = str -> ManifestType decl.manifest
          | Module_sig(id,mty) when (Ident.name id) = str -> Modtype mty
          | _ ->  (find_decl xs)
      in
      match mty with 
        | Signature sls -> (find_decl sls)
        | Functor_type(arg,_,_) when (Ident.name arg) = str -> Modtype mty (* TODO *)
        | Functor_type(_,ml1,ml2) -> match (find str ml1) with
          | Fail -> (find str ml2)
          | _ as a -> a
    in

    (* top level *)
    match path with | [] -> raise (Cannot_compile "Empty path given to lookup")
    | str :: [] -> (find str ty)
    | str :: ls -> match (find str ty) with 
      | Modtype mty -> (look_up_type_path mty ls)
      | _ -> raise (Cannot_compile "Reached a dead end in type path search")


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name: gen_full_path
  *  Description: generate a path to pth in mty using nm to check name space (* God mode *)
  * =====================================================================================
  *)
  let gen_full_path mty pth ns =

    (* establish the target *) 
    let target = (match (List.rev pth) with [] -> raise (Cannot_compile "Empty path to complete") | x::_ -> x) in

    (* search declarations *)
    let rec full_decl (topl : string list) (sigls : specification list) (ns : string list) : unit = match sigls with 
       | [] -> ()
       | x :: xs -> match x with 
         | Value_sig (id,_) when (Ident.name id) = target -> raise (Found (List.rev (pth@topl)))
         | Type_sig (id,_) when (Ident.name id) = target -> raise (Found (List.rev (pth@topl)))
         | Module_sig (id,mod_type) when (Ident.name id ) = target -> raise (Found (List.rev (pth@topl)))
         | Module_sig (id,mod_type) -> let name = (Ident.name id) in
           if (((List.length ns) > 0) && (name = (List.hd ns))) 
           then ((full_mty (name::topl) mod_type (List.tl ns)); (full_decl topl xs ns))  (* Recurse *)
           else (full_decl topl xs ns)
         | _ -> (full_decl topl xs ns)

    (* search module types *)
    and full_mty (topl : string list) (ty : mod_type) (ns : string list) : unit = match ty with
      | Signature sigls -> (full_decl topl (List.rev (sort_sigs sigls)) ns)
      | Functor_type (id,ml1,ml2) when (Ident.name id) = target -> raise (Found (List.rev (pth@topl)))
      | Functor_type (id,ml1,ml2) -> let argum = (Ident.name id) in
         (full_mty topl ml1 ns); (full_mty topl ml2 ns); ()
     
    in
    (* top level *)
    (*(Printf.eprintf "Looking for path: %s -> %s in %s\n" (String.concat "." pth) target (String.concat "." ns));*)
    (full_mty [] mty (List.rev ns))


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  compile_simple_type
  *  Description:  convert the simple type to the intermed representation
  * =====================================================================================
  *)
  let rec compile_simple_type progtype pth =  
    function
    | Var _ as x -> compile_simple_type progtype pth (typerepr x)
    | LambdaType(TIgnore,_) -> TyIgnore
    | LambdaType(TBool,_) -> TyBool
    | LambdaType(TInt,_) ->  TyInt
    | LambdaType(TArrow,[ty1;ty2]) -> let tu1 = compile_simple_type progtype pth ty1 in
        let tu2 = compile_simple_type progtype pth ty2 in
        TyArrow (tu1,tu2)
    | LambdaType(TPair,[ty1;ty2]) -> let tu1 = compile_simple_type progtype pth ty1 in
        let tu2 = compile_simple_type progtype pth ty2 in
        TyStar (tu1,tu2)
    | Typeconstr(path,_) -> (*(Printf.eprintf "Going for %s\n" (String.concat "." (convert_path path)));*)
      let obtainbase p = 
        (*Printf.eprintf "full path = %s\n" (String.concat "." p));*)
        (match (look_up_type_path progtype p) with
          | Fail -> raise (Cannot_compile "Type path not found")
          | SimpleType ty -> (compile_simple_type progtype p ty)
          | Modtype mty -> (compile_mty_type progtype (List.rev p) mty)
          | ManifestType opt -> (match opt with
            | None -> (TyAbstract (TyCString (String.concat "." p)))
            | Some simple -> (compile_simple_type progtype (List.rev p) simple.defbody)))
      in
      let fullpath = 
        (try (gen_full_path progtype (convert_path path) pth); 
           raise (Cannot_compile "Couldn't gen. path")
         with Found topl -> topl
         | Cannot_compile _ as a -> raise a) in
        (*(raise (Cannot_compile ("Before obtain "^(String.concat "." fullpath)))); *)
      (obtainbase fullpath)
      | _ -> raise (Cannot_compile "Cannot convert a simple type")
 

 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  parse_type
  *  Description:  compiles the lambda calculus
  * =====================================================================================
  *)
  and compile_mty_type progtype pth mty = 

    (* type declarations, without kind or param *)
    let convert_decl pth dec = match dec.manifest with
      | None -> TyAbstract (TyCString (String.concat "." (List.rev pth)))
      | Some dt -> (* TODO params *) (compile_simple_type progtype pth dt.defbody)
    in

    (* signature specifications *)
    let rec convert_spec pth = function
      | Value_sig (id,st) -> TyValue (TyCString (Ident.name id),(compile_simple_type progtype pth st.body))
      | Type_sig (id,td) -> TyDeclar (TyCString (Ident.name id),(convert_decl pth td))
      | Module_sig (id,mty) -> let name = (Ident.name id) in
        TyModule (TyCString (Ident.name id), compile_mty_type progtype (name::pth) mty)
    in

    (* top level *)
    match mty with 
    | Signature sigls -> TySignature (List.map (fun x -> convert_spec pth x) sigls)
    | Functor_type (id,mty1,mty2) -> let argum = (Ident.name id) in
      let inter1 = (compile_mty_type progtype pth mty1)
      and inter2 = (compile_mty_type progtype pth mty2) in
      TyFunctor (TyCString argum ,inter1,inter2)

 (* 
  * ===  FUNCTION  ======================================================================
  *     Name: type_weave
  *  Description:  converts the binding into lists of gettr's struct ptr's and fctr's 
  * =====================================================================================
  *)
  let type_weave progtype binding = 

    (* ================================================= *)
    (* extract the shared associations                   *)
    (* ================================================= *)
    let extract_assoc sign binding = 

      (* remove unwanted data from a signature specification *)
      let rec clear_sigs = function [] -> []
        | x :: xs -> (match x with
          | Type_sig _ -> (clear_sigs xs)
          | _ -> x :: xs)
      in

      (* remove those structure bindings that don't need to be shared *)
      let rec filter_shares sigls strls = match(sigls,strls) with ([],_) -> []
        | (s::ss,b::bs) -> (match (s,b) with
          | (Value_sig (id1,vty),BVal (name,_,_)) when (Ident.name id1) = name ->
            (*Printf.eprintf "%s == %s \n" (Ident.name id1) name) ;*)
            (b,s) :: (filter_shares ss bs) 
          | (Value_sig _, BVal _ ) -> (filter_shares (s::ss) bs)
          | (Module_sig (id1,mty), BMod (name,_)) when (Ident.name id1) = name ->
            (b,s) :: (filter_shares ss bs)
          | (Module_sig _, BMod _) -> (filter_shares (s::ss) bs)
          | (Module_sig (id1,_),BVal _) -> (filter_shares (s::ss) bs)
          | _ -> raise (Cannot_compile "Serieus failure of argument structure in Share computation"))
        | _ -> raise (Cannot_compile "Serieus failure of list structure in Share computation")
      in

      (* pipe in the easiest possible input *)
      let clear_input sign strls = match sign with 
        | Signature sigls -> (*(Printf.eprintf "%d vs %d\n" (List.length sigls) (List.length strls)); *)
            (filter_shares (clear_sigs (sort_sigs sigls)) (sort_bindings strls))
        | _ -> raise (Cannot_compile "Expected a Signature")
      in

      (* Do the conversion *)
      let rec convert_assoc path = function [] -> []
        | (bind,ty)::ls -> match (bind,ty) with
          | (BVal (name,_,_) , Value_sig (_, vty)) ->  
            (Call (vty,name,path))::(convert_assoc path ls) 
          | (BMod (name, modt), Module_sig(_,mty)) -> (match modt with 
            | SB (pth, nbinding,unique) -> let recurse = if unique 
              then (convert_assoc (name::path) (clear_input mty nbinding))
              else [] in 
                ((Share (mty,name,pth,path)) :: recurse) @ (convert_assoc path ls)
            | FB (pth,id,_,_) -> (Fu (mty,name,id,pth,path)) :: (convert_assoc path ls)) 
          | _ -> raise (Cannot_compile "Massive idiocy everywhere")

      in

      (* top level *)
      (Share (progtype,"this",[],[])) :: (convert_assoc [] (clear_input sign binding)) 
    in


    (* ================================================= *)
    (* Compile the entrypoints                           *)
    (* ================================================= *)
    let compile_entrypoints assocs =

      (* new_variables *)
      let new_var = let count = ref (-1) in 
        fun () -> incr count; (var_prefix^"_s_" ^ (string_of_int !count)) 
      in

      (* new mask *)
      let new_mask = let count = ref (-1) in
        (fun () -> incr count; !count) 
      in

      (* new mask *)
      let new_maskf = let count = ref (-1) in
        (fun () -> incr count; !count) 
      in

      (* build structure assignment *)
      let build_strc sty ty ls = 
        let rec convert s = function [] -> []
          | x :: xs -> match x with
            | Call (_,str,pth) -> SetMember(s,str,CVar (make_entrypoint (str::pth))) :: (convert s xs)
            | Share (_,str,[],[]) when str = "this" -> (convert s xs)
            | Share (_,str,opth,pth) -> SetMember(s,str, CVar (make_entrypoint (str::pth))) :: (convert s xs)
            | Fu (_,str,_,opth,pth) -> SetMember(s,str, CVar (make_entrypoint (str::pth))) :: (convert s xs)
        in
        let nv = new_var() in
        let nm =  new_mask() in
        let var = ToStatic (sty,(CVar nv)) in
        let mask = SetMember (nv,"mask",(CInt nm)) in
        let typ = SetMember (nv,"type",ToCall((constc CONT),[ty])) in
        let set = (convert nv ls) in
        let retv = CVar nv in
        ([],[var;mask;typ]@set,retv)
      in

      (* convert assocs into Gettrs *)
      let rec entrypts pls = function [] -> []
        | x :: xs -> match x with
          | (Call(ty,name,pth)) -> 
            let eptr = (make_entrypoint (name::pth))  in
            let ptr = make_ptr (name::pth) in
            let tycomp = CVar (printty (compile_simple_type progtype pth ty.body)) in
            let comp = (ToCall ((constc CONV),[ToCall((CVar ptr),[]) ; tycomp ])) in
            Gettr(eptr,(constd DATA),ENTRYPOINT,([],[],comp)) :: (entrypts pls xs)
          | Share(mty,name,opth,pth)  -> 
            let tail = (try (List.tl pls) with _ -> []) in
            let memb = (try (List.hd pls) with _ -> []) in
            let eptr = (make_entrypoint (name::pth)) in
            let ptr = make_ptr (name::pth) in
            let sty = (TyCStruct ptr) in
            let otherc = (make_entrypoint opth) in 
            if otherc = eptr  then 
              let modcomp = CVar (printty (compile_mty_type progtype opth mty)) in
              let compu = (build_strc sty modcomp memb) in
              Gettr(eptr,sty,ENTRYPOINT,compu) :: (entrypts tail xs)
            else if name = "this" then
              let special = (TyCStruct (make_ptr [])) in
              let modcomp = CVar (printty (compile_mty_type progtype opth mty)) in
              let compu = (build_strc special modcomp memb) in
              Gettr(name,special,ENTRYPOINT,compu) :: (entrypts tail xs)
            else
              let optr = make_ptr opth in
              let comp = ToCall ((CVar otherc),[]) in
              Gettr(eptr,(TyCStruct optr),ENTRYPOINT,([],[],comp)) :: (entrypts pls xs)
          | Fu(mty,name,id,opth,pth) -> 
            let eptr = (make_entrypoint (name::pth)) in
            let otherc = (make_entrypoint opth) in 
            if otherc = eptr then 
              (*let modcomp = CVar (printty (compile_mty_type progtype opth mty)) in*)
              let nv = new_var() in
              let nm = new_maskf() in
              let var = ToStatic((constd DATA),(CVar nv)) in
              let tag = SetMember (nv,"t",(CVar "FUNCTOR")) 
              and mask = SetMember (nv,"identifier",(CInt nm)) 
              and retv = CVar nv in
              Gettr(eptr,(constd DATA),ENTRYPOINT,([],[var;tag;mask],retv)) :: (entrypts pls xs)
            else 
              let comp = ToCall ((CVar otherc),[]) in
              Gettr(eptr,(constd DATA),ENTRYPOINT,([],[],comp)) :: (entrypts pls xs)
      in

      (* top level *)
      (entrypts (split_assocpth assocs) assocs)
    in

    (* top level *)
    let (gettrs,strcts,fctrs) = (MC.High.extract [] binding) 
    and assocs  = List.rev (sort_assocspth (extract_assoc progtype binding)) in 
    let gettr_s = (sort_compred gettrs) in
    let ngettrs = List.map (function Gettr(str,dtstr,_,comp) -> Gettr(str,dtstr,LOCAL,comp)) gettr_s in
    let nfctrs = List.map (function Fctr(p,_) -> Fctr(p,LOCAL)) fctrs in
    let gentry  = compile_entrypoints assocs in
      (gentry,ngettrs,strcts,nfctrs,assocs)


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  compile
  *  Description:  converts the toplevel into a tuple of 2 strings for object & header
  * =====================================================================================
  *)
  let compile mty program headerf =

    (* ================================================= *)
    (* build the bootup function: where we set it all up *)
    (* ================================================= *)
    let boot_up strls assocs progtype =

      (* create the insertion binding *)
      let to_binding = function [] -> (constv TOP)
        | x :: xs as ls -> let ptr = (make_ptr ls) in
          (CVar (ptr ^ "->mod" )) 
      in
       
      (* add print the associated binding *)
      let rec print_assoc = function [] -> ([],[])
        | Call (vty,strl,pth) :: xs -> 
          let strr = CVar (make_ptr (strl::pth)) in
          let statptr = (CVar (var_prefix^"_"^strl^"_str")) in
          let static = ToStatic((constd CHAR),Assign(Ptr statptr,(CString strl)))
          and temp = InsertMeta ((to_binding pth),statptr,strr,1, (compile_simple_type progtype pth vty.body)) in
          let (lss,lsb) = (print_assoc xs) in
          (static::lss,temp::lsb) 
        | Share (_,str,[],[]) :: xs when str = "this" -> (print_assoc xs)
        | Share (mty,strl,pth,_) :: xs -> 
          let strr = CVar (make_ptr pth) 
          and bindpth = try (List.tl pth) with _ -> []
          and statptr = (CVar (var_prefix^"_"^strl^"_str")) in
          let static = ToStatic((constd CHAR),Assign(Ptr statptr,(CString strl)))
          and tmpmty = TyModule ((TyCString strl),(compile_mty_type progtype pth mty)) in
          let temp = InsertMeta ((to_binding bindpth),statptr,strr,0,tmpmty) in
          let (lss,lsb) = (print_assoc xs) in
          (static::lss,temp::lsb) 
        | Fu _ :: xs -> (print_assoc xs)
      in

      (* print_strcts: convert structs into mallocs and bindings *)
      let rec print_strcts = function [] -> ([],[])
        | (Strct pth) :: xs -> (let ptr = (make_ptr pth) in
          let decl = MALLOC (STRUCTURE,(CVar ptr),(Sizeof STRUCTURE)) in
          let (dls,bls) = (print_strcts xs) in
            ((decl :: dls), bls)) 
        | _ -> raise (Cannot_compile "print_strcts only prints Strct") 
      in
        
      (* top level *)
      let def = ToDef ((CVar "int"),(constc BOOT),[]) in
      let strdecl = (match (print_strcts strls) with 
        | (a,b) -> (a @ [Emptyline] @ b)) in
      let (statics,bindings) = (print_assoc assocs) in
      let final_sts =  (List.sort_uniq cmp_stat statics) in
      let body_ls = (List.map printc (final_sts @ [Emptyline] @ strdecl @ bindings @ [Emptyline;(ToReturn (CInt 1))])) in
      ( (printc def) :: (format 1 body_ls) @ func_end) 
    in

    (* print fnctrs TODO *)
    let rec print_fctrs = function [] -> []
      | (Fctr (name,loc)) as f :: xs -> let definition = printf (MC.Low.funcdef false f) in
        let body = (format 1 ["return;"]) in 
        (String.concat "\n" ( (definition::body) @ func_end)) :: (print_fctrs xs) 
      | _ -> raise (Cannot_compile "print_fctrs - only compiles Gettr")
    in

    (* print_strc *)
    let  print_strc l = 
      let rec process ss = function [] -> []
        | ls :: lls -> 
          (try let first = (List.hd ls) in 
            let name = (make_ptr (match first with
              | Call (_,_,pth) -> pth
              | Share (_,_,_,pth) -> pth
              | Fu (_,_,_,_,pth) -> pth))
            in
            let rec convert = function [] -> []
              | x :: xs -> match x with
                | Call (_,str,pth) ->  CallMember ((constd DATA),str,[]) :: (convert xs)
                | Share (_,str,_,_) when str = "this" -> (convert xs)
                | Share (_,str,pth,_) -> let strr = (make_ptr pth) in
                  if (List.exists (fun x -> x = strr) ss) 
                  then CallMember ((TyCStruct strr),str,[]) :: (convert xs)
                  else raise (Cannot_compile "undefined structure")
                | Fu (_,str,_,pth,_) -> CallMember ((constd DATA),str,[]) :: (convert xs)
            in
            let defls = convert ls 
            and mask = Member ((TyCType "int"),"mask")
            and typ = Member ((constd DTYPE),"type")
            in
            (ToStructure (name, mask :: typ :: defls)) :: (process (name::ss) lls)
          with Failure s -> (process ss lls))
      in
      (* top level *)
      (process [] l)
    in
    
    (* convert list of compiler redices into strings of function definitions *)
    let mapfd ls = (List.map printf 
      (List.map (fun x -> (MC.Low.funcdef true x)) ls))
    in

    (* Top Level *)
    let (lambda_list,omega) = (MC.High.compile program) in
    let (gentry,gettr_lst,strct_list,fctr_list,assocs) = type_weave mty omega in 

    (* build the header *)
    let str_ls = (separate "Structs" (format 0 (List.map printc (print_strc (split_assocpth assocs)))))
    and en_dls = (separate "Entry Points" (mapfd gentry) )
    and hedh = header  (List.map printc [(consth ENTRY)]) in
    let headerfile = (String.concat "\n"(hedh @ str_ls @ en_dls)) ^ "\n"  
    in

    (* build the object file *)
    let dec_ls = (separate "declarations" (mapfd gettr_lst))
    and pl_ls = (separate "Closures" (MC.Low.lambda (List.rev lambda_list)))
    and pv_ls = (separate "Values" (MC.Low.getter (List.rev gettr_lst)))
    and en_ls = (separate "Entry Points" (MC.Low.getter gentry))
    and fc_ls = (separate "Functors" (print_fctrs (List.rev fctr_list)))
    and pb_ls = (separate "Boot" (boot_up strct_list assocs mty)) 
    and objh =  header (List.map printc [(Include headerf) ; (consth MINI)])
    in

    (* the two files *)
    let objectfile = ((String.concat "\n"  (objh @ dec_ls @ pl_ls @ pv_ls @ fc_ls @ en_ls @ pb_ls @ footer)) ^ "\n")
    in (objectfile,headerfile)
  
end

