(*
 * =====================================================================================
 *
 *     Filename:  compiler.ml
 *
 *  Description:  Compile the AST into C?
 *
 *     Author:  Adriaan Larmuseau, ajhl
 *    Company:  Uppsala IT
 *
 * =====================================================================================
 *)

open Mini
open Modules (*opening mini should open this *)


module CCompiler =
struct
  open MiniML 
  open MiniMLMod


  (* Exceptions *) 
  exception Cannot_compile of string

 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)

  (* types used during omega translation and lambda calc compilation *)
  type cpath = string list
  and modbindtype = FB of cpath * string * mod_term | SB of cpath * strctbinding list
  and strctbinding = BVal of string * computation | BMod of string * modbindtype 
  and computation = tempc list * tempc 
  and tempc = ToValue of tempc | ToInt of int | ToBoolean of int 
  
  (* types used during theta translation *)
  type compred = Gettr of string * computation | Strct of cpath * assoc list 
         | Fctr of cpath * assoc list | Compttr of string * computation * tempc list
  and assoc = action * string * string
  and action = Call | Share
 
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

 (*-----------------------------------------------------------------------------
  *  Global constants
  *-----------------------------------------------------------------------------*)

  let CONST_ENV = "my_env" 
  and CONS_ARG = "my_arg"
  and CONST_MOD = "my_mod" 


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  lookup_path
  *  Description: Fetches the environent of list of paths
  * =====================================================================================
  *)
  let rec lookup_path env path = 

    (* look up x in the env *)
    let get_binding x = 
      let find_binding = (function
      | BVal (nn,_) when (nn = x) -> true
      | BMod (nn,_) when (nn = x) -> true
      | _ -> false) in
      try (List.find (find_binding) env)
      with _ -> raise (Cannot_compile "Identifier not found") in

    match path with  x::[] -> let BMod (_,e) = (get_binding x env) in e
      | x::xs -> let SB (_,nenv) =  (lookup_path env xs) in
        let BMod (_,e) = (get_binding x nenv) in e 


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  parse_computation
  *  Description:  compiles the lambda calculus
  * =====================================================================================
  *)
  let rec parse_computation env pathstr funclist program = 

    (*TODO generate v@r based on existing syms *)
    let new_var = let count = ref (-1) in 
      fun () -> incr count; ("v@r_" ^ (string_of_int !count)) in

    (* get rid of the let terms *)
    let rec desugar = function
      | Let (id,e,t) -> Apply( (desugar (Function(id, t))) , (desugar e))
      | Constant _ as t -> t
      | Boolean _ as t -> t
      | Longident _ as t -> t
      | If (a,b,c) -> If ((desugar a),(desugar b),(desugar c))
      | Prim (op,ls) -> let nls = (List.map desugar ls) in Prim(op,nls)
      | Function(id,e) -> Function (id,desugar e) in
     
     (* miniml -> interm + var list side effect *)
     let rec compile varlist program = 
      let rec convert = function 
        | Longident path -> try get_binding 
        | Constant x -> ToInt x
        | Boolean x -> ToBoolean (match x with | true -> 1 | _ -> 0)
        | If (a,b,c) -> (Question (ToValue (convert a)),(convert b),(convert c) )
        | Pair(a,b) -> (ToPair (convert  a), (convert b))
        | Apply (l,r) -> let tmp = new_var() in 
        varlist := tmp :: !varlist;
        let tcv = (CVar tmp) in
        ToComma((Assign tcv, (compile l)),(ToCall ((ToLambda tcv),[(ToEnv tcv);(ToMod tcv); (convert r)])))
        | Function(id,e) -> let idn = (Ident.name id) in
        let lamname = pathstr^idn in
        (makef lamname idn e);
        ToClosure((CVar CONST_ENV),(CVar CONST_MOD),(CVar lamname))
        | _ -> raise (Cannot_compile "Failed to wipe out the lets") in
      (convert program)

      (* create a lambda function *)
      and makef name id e = let vlist = ref [] in
      let compiled = compile vlist e in
      let compttr = Compttr (name,(!vlist,compiled),[(Insert((CVar CONST_ENV),(CVar id),(CVar CONS_ARG)))]) in
      funclist := compttr :: !funclist in

    (* toplevel *)
    let var_list = ref [] in
    let computation = compile var_list program in (!var_list,computation)

   (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  printc
  *  Description:  When all is said and done, conver the computation to string
  * =====================================================================================
  *)
  let rec printc = function
    | ToValue a -> (printc c)^".value"  (* TODO is ptr ? *)
    | ToInt x -> "MakeInt(" ^ x ^ ")"
    | ToBoolean x -> "MakeBoolean("^ x ")"
    | ToQuestion(a,b,c) -> "("^(printc a)^" ? "^(printc b)^" : "^(printc c)^")"
    | ToPair (a,b) -> "MakePair("^(printc a)^","^(printc b)^")"
    | ToComma(a,b) -> "("^(printc a)^","^(printc b)^")"
    | Assign (a,b) -> (printc a) ^ "=" ^ (printc b)
    | CVar str -> str
    | ToCall (f,ls) -> (printc f)^"("^(String.concat "," (List.map printc ls))^")"
    | ToLambda a -> (printc a)^".lam"
    | ToEnv a -> (printc a)^".env"
    | ToMod a -> (printc c)^".mod"
    | Insert (a,b,c) -> "insertbinding("^(printc a)^","^(printc b)^","^(printc c)^",0)"
    | ToClosure (a,b,c) -> "makeClosure("(printc a)^","^(printc b)^","^(printc c)^")"
  
       (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  omega_transformation
  *  Description:  converts the toplevel into an omega binding
  * =====================================================================================
  *)
  let omega_transformation program = 

    (* convert a sequence of structure definitions *)
    let rec parse_struct env path strctls   = 

      (* convert a module definition into a new environment *)
      let rec parse_module env pth modterm = 

      
        match modterm with Longident ident -> (lookup_path env (convert_path ident))
          | Structure strls -> let parsed = (parse_struct env pth strls) in SB (pth,parsed)
          | Functor (id,ty,m) -> FB (path,(Ident.name id),m)
          | Apply (m1,m2) -> 
            (match (parse_module env pth m1) with
              | FB (_,id,m) -> let nenv = (parse_module env pth m2) in
                (parse_module ((BMod (id,nenv))::env) pth m)
              | _ -> raise (Cannot_compile "Needed Functor"))
          | Constraint (m,ty) -> raise (Cannot_compile "Constraint !") in

      (* recurse over the list of definitions *)
      match strctls with [] -> []
        | x::xs ->  match x with Type_str _ -> (parse_struct env path xs)
          | Module_str (id,mterm) -> 
            let name = (Ident.name id) in
            let nenv = (parse_module env (name::path) mterm) in
            let value = BMod ((Ident.name id),nenv) in
            value :: (parse_struct (value :: env) path xs)
          | Value_str (id,term) -> 
            let comp = (parse_computation env term) in
            let data = BVal ((Ident.name id), (comp)) in 
              data :: (parse_struct (data :: env) path xs) in
        
    (* top level *)
    match program with Structure strt -> (parse_struct [] [] strt) 
    | _ ->  
      raise (Cannot_compile "Top level must be structure")

   (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  theta_transformation
  *  Description:  converts the binding into lists of gettr's struct ptr's and fctr's 
  * =====================================================================================
  *)
  let theta_transformation binding = 

    (* the collection for the compilation *)
    let gettr_lst = ref []
    and strct_list = ref []
    and fctr_list = ref [] 
    and path_list = ref [] in

    (* helpers functions *)
    let find_path x lst = (List.exists (function y -> (make_ptr x) = y) lst) in

    (* select the structures gettrs from the previoulsy obtained omega *)
    let rec select path = (function [] -> []
      | BVal (name, comp) :: ls -> let ptr = make_ptr (name::path) in
        gettr_lst := ((Gettr (ptr,comp)) :: !gettr_lst);
        (Call,name,ptr)::(select path  ls) 
      | BMod (name, modt) :: ls -> match modt with
        | SB (pth,nbinding) when (not (find_path pth !path_list)) ->  
          path_list := (make_ptr pth) :: !path_list;
          let light_list = (select (name::path) nbinding) in
          let ptr = (make_ptr pth) in
          strct_list := ((Strct (pth,light_list)) :: !strct_list);
          (Share,name,ptr)::(select path ls)
        | SB (pth,nbinding) -> (Share,name,(make_ptr pth))::(select path ls)
        | FB (pth,var,m) -> 
          fctr_list :=  !fctr_list;
          (Share,name,(make_ptr pth))::(select path ls)) in 

    (* top level *)
    let top_level = (select [] binding) in 
    (!gettr_lst,!strct_list,!fctr_list,top_level)
     

   (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  compile
  *  Description:  converts the toplevel into a giant string
  * =====================================================================================
  *)
  let compile program =

    (* add associations to bindings *)
    let rec print_assoc binding = function [] -> []
      | (ty,strl,strr)::xs -> let ui = (match ty with Call -> "1" | _ -> "0") in
        let temp = ("insertBinding(&"^binding^", \""^strl^"\" , "^strr^", "^ui^");") in
        temp :: (print_assoc binding xs) in

    (* print_strcts: convert structs into mallocs and bindings *)
    let rec print_strcts = function [] -> ([],[])
      | Strct (pth,assocs) :: xs -> 
        let ptr = (make_ptr pth) in
        let binding = (getmod ptr) in
        let decl = ("struct Structure * "^ptr^" = malloc(sizeof(struct Structure));") in
        let parent = try let tail = (List.tl pth) in match tail with
          | [] -> []
          | x::xs -> [(binding^" = "^(getmod (make_ptr tail))^";")] 
          with _ -> [] in
        let (dls,bls) = (print_strcts xs) in
          ((decl :: dls), (parent @ (print_assoc binding assocs) @ bls)) in

    (* build the bootup function: where we set it all up *)
    let boot_up strls top =
      let def = "LOCAL int bootup(void) {\n"  in
      let body = (match (print_strcts strls) with 
        | (a,b) -> (a @ [""] @ b)
        |  _ ->  raise (Cannot_compile "Can't combo a tuple")) in 
      let body2 = (print_assoc "toplevel"  top) in
      (def :: (body @ body2) @ ["";"return 1;";"}"]) in

    let rec print_computation comp = ["VALUE p = {.byte = NULL}; ";"return p;"] in

    let rec print_getters = function [] -> []
      | Gettr  (ptr,comp) :: xs -> let definition = ("VALUE "^ptr^"(BINDING * mod){") in
        let body = (String.concat "\n" (print_computation comp)) in
        (String.concat "\n" (definition::body::"}\n"::[]) ) :: (print_getters xs) in

    (* header and footer for the compiled result *)
    let header = ["// Compiled by lanren"; "#include \"miniml.h\"";  ""] in
    let footer = ["";"// Include the entrypoints & binding code"; "#include \"binding.c\""; "#include \"data.c\"" ; 
            "#include \"entry.c\""; ""] in

    (* Top Level *)
    let omega = omega_transformation program in
    let (gettr_lst,strct_list,fctr_list,top_level) = theta_transformation omega in 
    ((String.concat "\n"  (header @ (print_getters (List.rev gettr_lst)) @ (boot_up (strct_list) top_level ) @ footer)) ^ "\n")
  
end


