(*
 * =====================================================================================
 *
 *     Filename:  compiler.ml
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


(* Exceptions *) 
exception Cannot_compile of string


(* ===  MODULE  ======================================================================
 *         Name:  CIntermediary
 *  Description:  defines and prints into the definitions of miniml.h
 * =====================================================================================
 *)
module CIntermediary =
struct

 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)

  type type_u = TyInt | TyIgnore | TyBool | TyArrow of type_u * type_u 
    | TyStar of type_u * type_u

  type tempc = ToBValue of tempc | ToIValue of tempc | ToInt of tempc | ToBoolean of tempc | CVar of string 
    | ToQuestion of tempc * tempc * tempc | ToPair of tempc * tempc | ToComma of tempc * tempc
    | Assign of tempc * tempc | ToCall of tempc * tempc list | ToLambda of tempc | CInt of int
    | ToEnv of tempc | ToMod of tempc | Insert of tempc * tempc * tempc | ToCast of string * tempc
    | ToClosure of tempc * tempc  | Get of tempc * tempc | CString of string | CastMAX of tempc
    | MALLOC of tempc *tempc * tempc | Ptr of tempc | Adress of tempc | ToByte of tempc 
    | ToOper of string * tempc * tempc | ToLeft of tempc | ToRight of tempc | Sizeof of tempc
    | ToStatic of tempc * string | Emptyline | ToReturn of tempc | ToDef of tempc * tempc * tempc list
    | InsertMeta of tempc * tempc * tempc * int * type_u


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
  let rec printty : type_u -> string = function TyIgnore -> "makeTIGNORE()"
    | TyInt -> "makeTInt()"
    | TyBool -> "makeTBoolean()"
    | TyArrow (a,b) -> "makeTArrow("^(printty a)^","^(printty b)^")"
    | TyStar (a,b) -> "makeTStar("^(printty a)^","^(printty b)^")"


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  printc
  *  Description:  When all is said and done, conver the computation to string
  * =====================================================================================
  *)
  let rec printc : tempc -> string = function ToBValue a -> (printc a)^".b.value"  (* TODO is ptr ? *)
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
    | InsertMeta (a,b,c,d,e) -> "insertBigBinding("^(printc (Adress a))^","^(printc b)^","^(printc c)^","^
      (printc (CInt d))^","^(printty e)^")"

end


(* ===  MODULE  ======================================================================
 *         Name:  CCOMPILER
 *  Description:  The secure Compiler
 * =====================================================================================
 *)
module type CCOMPILER =
sig
  val compile: MiniMLMod.mod_type -> MiniMLMod.mod_term -> string
end 

module CCompiler : CCOMPILER =
struct

  open MiniML 
  open MiniMLMod
  open CIntermediary


 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)

  (* types used during omega translation and lambda calc compilation *)
  type cpath = string list

  and modbindtype = FB of cpath * string * mod_term * bool| SB of cpath * strctbinding list * bool

  and strctbinding = BVal of string * cpath * computation | BMod of string * modbindtype 

  and computation = tempc list * tempc 

  and trawl = Static of string | Environment of modbindtype

  (* types used during theta translation *)
  type compred = Gettr of string * computation | Strct of cpath 
               | Fctr of string | Compttr of string * computation * tempc list
  (* Bindings *)
  and assoc = Call of val_type * string * string list
            | Share of MiniMLMod.mod_type * string * string list
 

 (*-----------------------------------------------------------------------------
  *  Helper Funcions
  *-----------------------------------------------------------------------------*)

  (* convert a path into a list of strings *)
  let rec convert_path = (function Pident id -> [Ident.name id]
    | Pdot (p,str) -> str :: (convert_path p)) 
  
  (* convert a list into a pointer *)
  let make_ptr lst = (String.concat "_" (List.rev lst)) 

  (* simple string modification *)
  let getmod s = s^"->mod" 

  (* range operator *)
  let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc) in 
    (aux j [])

  (* get option operator *)
  let (+&) = (function Some a -> a | None -> raise (Cannot_compile "Some value doesn't exist"))


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  lookup_path
  *  Description: Fetches the environent of list of paths
  * =====================================================================================
  *)
  let rec lookup_path env path = 

    (* note that this is not the original one *)
    let set_origin = function 
      | FB (a,b,c,_) -> FB(a,b,c,false)
      | SB (a,b,_) -> SB(a,b,false)
    in

    (* look up x in the env *)
    let get_binding x nenv = 
      let find_binding = (function
      | BVal (nn,_,_) when (nn = x) -> true
      | BMod (nn,_) when (nn = x) -> true
      | _ ->  false) in
      try (List.find (find_binding) nenv)
      with Not_found -> raise (Cannot_compile "Identifier not found") in

    (* When it comes to values all that matters is that they exist *)
    let extract = function
        | BVal (nn,pth,_) -> Static (make_ptr (nn::pth))
        | BMod (_,e) -> (Environment (set_origin e)) in

    (* toplevel *)
    match path with  | [] -> raise (Cannot_compile "Empty path given to lookup")
      | x::[] -> (extract (get_binding x env)) 
      | x::xs -> match (lookup_path env xs) with
        | Environment( SB (_,nenv,_)) -> (lookup_path nenv (x::[]))
        | _ -> raise (Cannot_compile "Wrong tree structure") 

 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  parse_type
  *  Description:  compiles the lambda calculus
  * =====================================================================================
  *)
  let rec parse_type vty = 

    (* conver the simple types *)
    let rec convert_simple_type = function
      | Var _ as x -> convert_simple_type (typerepr x)
      | LambdaType(TIgnore,_) -> TyIgnore
      | LambdaType(TBool,_) -> TyBool
      | LambdaType(TInt,_) -> TyInt
      | LambdaType(TArrow,[ty1;ty2]) -> let tu1 = convert_simple_type ty1 in
          let tu2 = convert_simple_type ty2 in
          TyArrow (tu1,tu2)
      | LambdaType(TPair,[ty1;ty2]) -> let tu1 = convert_simple_type ty1 in
          let tu2 = convert_simple_type ty2 in
          TyStar (tu1,tu2)
      | Typeconstr(path,_) -> raise (Cannot_compile "When do we get paths?")
      | _ -> raise (Cannot_compile "Cannot convert a simple type")
    in

    (* toplevel *)
    convert_simple_type vty.body

 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  parse_computation
  *  Description:  compiles the lambda calculus
  * =====================================================================================
  *)
  let rec parse_computation env path funclist program = 

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
    let rec compile varlist program = 
      let is_intop str = 
        let findstr str2 = match (String.compare str str2) with 0 -> true | _ -> false in
        (List.exists findstr int_op) in
      let rec convert : MiniML.term -> tempc = function 
        | Longident lpath -> let cpath = (convert_path lpath) in 
           (try (match (lookup_path env cpath) with
              | Static spath -> ToCast (c_value,(ToCall ((CVar spath),[]))) 
              | _ -> raise (Cannot_compile "Did not retrieve path from lookup"))
           with _ -> (Get ((CVar const_env),(CString (make_ptr cpath)))))
        | Constant x -> ToInt (CInt x)
        | Boolean x -> ToBoolean (CInt (match x with | true -> 1 | _ -> 0))
        | If (a,b,c) -> ToQuestion ((ToBValue (convert a)),(convert b),(convert c))
        | Pair(a,b) -> ToPair ((convert  a), (convert b))
        | Apply (l,r) -> let tmp = new_var() in 
           varlist := (CVar tmp) :: !varlist;
           let tcv = (CVar tmp) in
           ToComma(Assign( tcv, (convert l)),ToCast (c_value,(ToCall ((ToLambda tcv),[(ToEnv tcv); (convert r)]))))
        | Function(id,ty,e) -> let idn = (Ident.name id) in
          let lamname = (new_func (make_ptr path))  in
          (*let convert_ty = (parse_type ty) in*)
          (makef lamname idn e);
          ToClosure((CVar const_env),(*convert_ty,*)(CVar lamname)) (* TODO pain point *)
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
        let compiled = compile vlist e in
        let ptrarg = CVar (new_ptr()) in
        let ptrstr = CVar (new_ptr()) in
        let malla = MALLOC ((CVar c_value),ptrarg,(Sizeof (CVar c_value))) in
        let assarg = Assign((Ptr ptrarg),(CVar const_arg)) in
        let asstr = ToStatic(ptrstr,id) in
        let insert = (Insert((CVar const_env),ptrstr,ptrarg)) in
        let compttr = Compttr (name,(!vlist,compiled),[asstr; malla ; assarg; insert]) in
        funclist := compttr :: !funclist in

    (* toplevel *)
    let var_list = ref [] in
    let computation = (compile var_list (desugar program)) in 
    (!var_list,computation)



 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  omega_transformation
  *  Description:  converts the toplevel into an omega binding
  * =====================================================================================
  *)
  let omega_transformation program = 

    let functlist = ref [] in

    (* convert a sequence of structure definitions *)
    let rec parse_struct env path strctls   = 

      (* convert a module definition into a new environment *)
      let rec parse_module env pth = function
          Longident ident -> (match (lookup_path env (convert_path ident)) with 
            | (Environment e) -> e
            | _ -> raise (Cannot_compile "Did not retrieve environment from path lookup"))
        | Structure strls -> let parsed = (parse_struct env pth strls) in SB (pth,parsed,true)
        | Functor (id,ty,m) -> FB (pth,(Ident.name id),m,true)
        | Apply (m1,m2) -> 
          (match (parse_module env pth m1) with
            | FB (_,id,m,_) -> let nenv = (parse_module env pth m2) in
              (parse_module ((BMod (id,nenv))::env) pth m)
            | _ -> raise (Cannot_compile "Needed Functor"))
        | Constraint (m,ty) -> (parse_module env pth m) (* TODO fix ! *)
      in

      (* recurse over the list of definitions *)
      match strctls with [] -> []
        | x::xs ->  match x with 
            Type_str (id2,_,_) -> (parse_struct env path xs)
          | Module_str (id2,mterm) ->
            let name = (Ident.name id2) in
            let nenv = (parse_module env (name::path) mterm) in
            let value = BMod (name,nenv) in
            value :: (parse_struct (value :: env) path xs)
          | Value_str (id2,term) ->
            let name = (Ident.name id2) in
            let comp = (parse_computation env (name::path) functlist term) in 
            let data = BVal (name, path, (comp)) in 
              data :: (parse_struct (data :: env) path xs) 
    in
        
    (* top level *)
    match program with Structure strt -> (!functlist,(parse_struct [] [] strt))
    | _ ->  raise (Cannot_compile "Top level must be structure")


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  theta_transformation
  *  Description:  converts the binding into lists of gettr's struct ptr's and fctr's 
  * =====================================================================================
  *)
  let theta_transformation progtype binding = 

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
           
      (* sort bindings *)
      let sort_bindings bls = 
        let cmp_binding a b = match (a,b) with
          | (BVal (name1,_,_) , BVal(name2,_,_)) -> (String.compare name1 name2)
          | (BMod (name1,_) , BMod(name2,_)) -> (String.compare name1 name2)
          | (BVal _, _) -> -1
          | (BMod _, _) -> 1
        in
        (List.sort cmp_binding bls)
      in

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
          | (Module_sig (id1,_),BVal _) -> (*Printf.eprintf "as I guessed = %s\n" (Ident.name id1)) ;*) (filter_shares (s::ss) bs)
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
                ((Share (mty,name,pth)) :: recurse) @ (convert_assoc path ls)
            | FB (pth,_,_,_) -> (Share (mty,name,pth)) :: (convert_assoc path ls)) 
          | _ -> raise (Cannot_compile "Massive idiocy everywhere")

      in

      (* top level *)
      convert_assoc [] (clear_input sign binding)
    in

    (* ================================================= *)
    (* extract the compilation redices need              *)
    (* ================================================= *)
    let rec extract_compred path = function [] -> ([],[],[]) 
      | str::ls -> (match str with
        | BVal (name, _, comp) -> let ptr = make_ptr (name::path) 
          and (a,b,c) = (extract_compred path ls) in
            ((Gettr (ptr,comp)) :: a, b, c)
        | BMod (name, modt) -> (match modt with
          | SB (pth,nbinding,true) ->  let (aa,bb,cc) = (extract_compred (name::path) nbinding)  
            and (a,b,c) = (extract_compred path ls) in
              ( aa @ a, (Strct pth) :: bb @ b, cc @ c) 
          | SB (pth,nbinding,false) -> (extract_compred path ls)
          | FB (pth,var,mm,_) -> let npth = (make_ptr pth) 
            and (a,b,c) =  (extract_compred path ls) in
              (a, b, (Fctr npth) :: c)))
    in 

    (* top level *)
    let (gettrs,strcts,fctrs) = (extract_compred [] binding) 
    and assocs = (extract_assoc progtype binding) in 
      (gettrs,strcts,fctrs,assocs)


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  compile
  *  Description:  converts the toplevel into a giant string
  * =====================================================================================
  *)
  let compile mty program =

    (* add ; and indentation *)
    let format n ls = let indent = (String.concat "" (List.map (fun x -> " ") (1 -- n))) in
        (List.map (fun x  -> if (not (x = "")) then (indent ^ x ^ ";") else "" ) ls) in

    (* end of function *)
    let func_end = ("}\n"::[]) in

    (* compare statics *)
    let comp_stat a b = match a with 
      ToStatic (_,x) -> (match b with 
        ToStatic(_,y) -> (String.compare x y)
        | _ -> -1) 
      | _ -> -1 in

    (* ================================================= *)
    (* build the bootup function: where we set it all up *)
    (* ================================================= *)
    let boot_up strls assocs =

      (* create the insertion binding *)
      let to_binding = function [] -> (CVar c_topl)
        | x :: xs as ls -> let ptr = (make_ptr ls) in
          (CVar (getmod ptr)) 
      in
       
      (* add print the associated binding *)
      let rec print_assoc = function [] -> ([],[])
        | Call (ty,strl,pth) :: xs -> 
          let strr = CVar (make_ptr (strl::pth)) in
          let statptr = (CVar (var_prefix^"_"^strl^"_str")) in
          let static = ToStatic(statptr,strl) 
          and temp = InsertMeta ((to_binding pth),statptr,strr,1, parse_type ty) in
          let (lss,lsb) = (print_assoc xs) in
          (static::lss,temp::lsb) 
        | Share (ty,strl,pth) :: xs -> 
          let strr = CVar (make_ptr pth) 
          and bindpth = try (List.tl pth) with _ -> []
          and statptr = (CVar (var_prefix^"_"^strl^"_str")) in
          let static = ToStatic(statptr,strl) 
          and temp = InsertMeta ((to_binding bindpth),statptr,strr,0,TyIgnore) in
          let (lss,lsb) = (print_assoc xs) in
          (static::lss,temp::lsb) 
      in

      (* print_strcts: convert structs into mallocs and bindings *)
      let rec print_strcts = function [] -> ([],[])
        | (Strct pth) :: xs -> (let ptr = (make_ptr pth) in
          let cvar = (CVar c_strc) in
          let decl = MALLOC (cvar,(CVar ptr),(Sizeof cvar)) in
          (*
          let binding = (CVar (getmod ptr)) in
          let parent = (try let tail = (List.tl pth) in match tail with
            | [] -> []
            | x::xs -> [Assign (binding,(CVar (getmod (make_ptr tail))))] 
            with _ -> []) in *) (* Note this is no longer necessary due to static comp *)
          let (dls,bls) = (print_strcts xs) in
            ((decl :: dls), bls)) 
        | _ -> raise (Cannot_compile "print_strcts only prints Strct") 
      in
        
      (* top level *)
      let def = ToDef ((CVar "int"),(CVar c_boot),[]) in
      let strdecl = (match (print_strcts strls) with 
        | (a,b) -> (a @ [Emptyline] @ b)) in
      let (statics,bindings) = (print_assoc assocs) in
      let final_sts =  (List.sort_uniq comp_stat statics) in
      let body_ls = (List.map printc (final_sts @ [Emptyline] @ strdecl @ bindings @ [Emptyline;(ToReturn (CInt 1))])) in
      ( (printc def) :: (format 1 body_ls) @ func_end) 
    in

    (* print the tuple computation *)
    let rec print_computation = function (vlist,comp) -> 
      let c = [("return "^(printc comp))] in
      let v = match vlist with [] -> []
        | _ -> [c_value^ " " ^(String.concat "," (List.map printc  vlist))] in
      v@c in

    (* print the lambda's*)
    let rec print_lambdas = function [] -> []
      | Compttr (name,comp,setup) :: xs -> let arguments = "("^c_binding^" "^const_env^", "^c_value^" "^const_arg^")" in
        let definition = (c_value^" "^name^arguments^"{") in
        let setupls : string list = (List.map printc setup)  in
        let body = (format 1 (setupls @ (print_computation comp))) in
        (String.concat "\n" ( (definition::body) @ func_end)) :: (print_lambdas xs) 
      | _ -> raise (Cannot_compile "print_lambdas - only compiles Compttr")
    in

    (* print a gettr *)
    let rec print_getters = function [] -> []
      | Gettr  (ptr,comp) :: xs -> let definition = ("LOCAL "^c_value^" "^ptr^"(void){") in
        let setup = c_binding^" "^const_env^" = NULL" in
        let body = (format 1 (setup :: (print_computation comp))) in
        (String.concat "\n" ( (definition::body) @ func_end ) ) :: (print_getters xs) 
      | _ -> raise (Cannot_compile "print_getters - only compiles Gettr")
    in

    (* print some local declarations *)
    let rec print_decl = function [] -> [] 
      | Gettr  (ptr,comp) :: xs -> ("LOCAL "^c_value^" "^ptr^"(void);") :: (print_decl xs) 
      | _ -> raise (Cannot_compile "print_decl - only compiles Gettr")
    in

    (* print fnctrs TODO *)
    let rec print_fctrs = function [] -> []
      | (Fctr name) :: xs -> let definition = c_strc^"* "^name^"("^c_strc^"* "^const_str^"){" in
        let body = (format 1 ["return NULL;"]) in 
        (String.concat "\n" ( (definition::body) @ func_end)) :: (print_fctrs xs) 
      | _ -> raise (Cannot_compile "print_decl - only compiles Gettr")
    in

    (* header and footer for the compiled result *)
    let header = ["// Compiled by lanren"; "#include \"miniml.h\"";  ""] in
    let separate name = function [] -> []
      | x::xs as ls -> ["//---------------"^ name ^ "----------------------"; ""] @ ls @ [""] in
    let footer = ["";"// Include the entrypoints & binding code"; "#include \"binding.c\""; "#include \"data.c\"" ; 
            "#include \"entry.c\""; ""] in

    (* Top Level *)
    (*var_prefix := (gen_rand 10);*)
    (*Printer.Pretty.print_modtype mty;*)
    let (lambda_list,omega) = omega_transformation program in
    let (gettr_lst,strct_list,fctr_list,assocs) = theta_transformation mty omega in 
    let dec_ls = (separate "declarations" (print_decl gettr_lst))
    and pl_ls =  (separate "Closures" (print_lambdas (List.rev lambda_list)))
    and pv_ls =  (separate "Values" (print_getters (List.rev gettr_lst)))
    and fc_ls = (separate "Functors" (print_fctrs (List.rev fctr_list)))
    and pb_ls = (separate "Boot" (boot_up strct_list assocs)) in
    ((String.concat "\n"  (header @ dec_ls @ pl_ls @ pv_ls @ fc_ls @ pb_ls @ footer)) ^ "\n")
  
end



