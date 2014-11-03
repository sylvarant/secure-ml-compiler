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
open Modules 


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
  and strctbinding = BVal of string * cpath * computation | BMod of string * modbindtype 
  and computation = tempc list * tempc 
  and tempc = ToBValue of tempc | ToIValue of tempc | ToInt of tempc | ToBoolean of tempc | CVar of string | CInt of int
            | ToQuestion of tempc * tempc * tempc | ToPair of tempc * tempc | ToComma of tempc * tempc
            | Assign of tempc * tempc | ToCall of tempc * tempc list | ToLambda of tempc
            | ToEnv of tempc | ToMod of tempc | Insert of tempc * tempc * tempc | ToCast of string * tempc
            | ToClosure of tempc * tempc  | Get of tempc * tempc | CString of string | CastMAX of tempc
            | MALLOC of tempc *tempc * tempc | Ptr of tempc | Adress of tempc | ToByte of tempc 
            | ToOper of string * tempc * tempc | ToLeft of tempc | ToRight of tempc | Sizeof of tempc
  and trawl = Static of string | Environment of modbindtype
  
  (* types used during theta translation *)
  type compred = Gettr of string * computation | Strct of cpath * assoc list 
               | Fctr of string | Compttr of string * computation * tempc list
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

  (* range operator *)
  let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc) in 
    (aux j [])

  (* get option operator *)
  let (+&) = (function Some a -> a | None -> raise (Cannot_compile "Some value doesn't exist"))

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


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  lookup_path
  *  Description: Fetches the environent of list of paths
  * =====================================================================================
  *)
  let rec lookup_path env path = 

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
        | BMod (_,e) -> (Environment e) in

    (* toplevel *)
    match path with  x::[] -> (extract (get_binding x env)) 
      | x::xs -> let Environment( SB (_,nenv)) = (lookup_path env xs) in
        (lookup_path nenv (x::[]))


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  parse_computation
  *  Description:  compiles the lambda calculus
  * =====================================================================================
  *)
  let rec parse_computation env path funclist program = 

    (*TODO generate v@r based on existing syms *)
    let new_var = let count = ref (-1) in 
      fun () -> incr count; ("v_r_" ^ (string_of_int !count)) in

    let new_lambda = let count = ref (-1) in
        fun () -> incr count; ("Lam_"^(string_of_int !count)) in

    let new_func str = (str^new_lambda())  in

    let new_ptr = let count = ref (-1) in
        fun () -> incr count; ("ptr" ^ (string_of_int !count)) in

    (* get rid of the let terms *)
    let rec desugar : MiniML.term -> MiniML.term  = function
      | Let (id,e,t) -> Apply( (desugar (Function(id, t))) , (desugar e))
      | Constant _ as t -> t
      | Boolean _ as t -> t
      | Longident _ as t -> t
      | If (a,b,c) -> If ((desugar a),(desugar b),(desugar c))
      | Prim (op,ls) -> let nls = (List.map desugar ls) in Prim(op,nls)
      | Function(id,e) -> Function (id,desugar e) 
      | Pair(a,b) -> Pair( (desugar a), (desugar b))
      | Prim (s,ls) -> Prim (s, (List.map desugar ls)) 
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
           (try let Static spath = (lookup_path env cpath) in 
             ToCast (c_value,(ToCall ((CVar spath),[]))) (*(Get ((CVar const_mod),(CString (make_ptr cpath))))*)
           with _ -> (Get ((CVar const_env),(CString (make_ptr cpath)))))
        | Constant x -> ToInt (CInt x)
        | Boolean x -> ToBoolean (CInt (match x with | true -> 1 | _ -> 0))
        | If (a,b,c) -> ToQuestion ((ToBValue (convert a)),(convert b),(convert c))
        | Pair(a,b) -> ToPair ((convert  a), (convert b))
        | Apply (l,r) -> let tmp = new_var() in 
           varlist := (CVar tmp) :: !varlist;
           let tcv = (CVar tmp) in
           ToComma(Assign( tcv, (convert l)),ToCast (c_value,(ToCall ((ToLambda tcv),[(ToEnv tcv); (convert r)]))))
        | Function(id,e) -> let idn = (Ident.name id) in
          let lamname = (new_func (make_ptr path))  in
          (makef lamname idn e);
          ToClosure((CVar const_env),(CVar lamname))
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
        let idsize = (String.length id)  in
        let malla = MALLOC ((CVar c_value),ptrarg,(Sizeof (CVar c_value))) in
        let malls = MALLOC ((CVar "char"),ptrstr,(CInt (idsize +1)))  in
        let assarg = Assign((Ptr ptrarg),(CVar const_arg)) in
        let assstr = ToCall ((CVar c_strcpy), [ptrstr; (CString id); (CInt idsize)]) in
        let insert = (Insert((Adress (CVar const_env)),ptrstr,ptrarg)) in
        let compttr = Compttr (name,(!vlist,compiled),[ malla ; malls ; assarg; assstr; insert]) in
        funclist := compttr :: !funclist in

    (* toplevel *)
    let var_list = ref [] in
    let computation = (compile var_list (desugar program)) in 
    (!var_list,computation)

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
    | Insert (a,b,c) -> "insertBinding("^(printc a)^","^(printc b)^","^(printc c)^",0)"
    | ToClosure (a,b) -> "makeClosure("^(printc a)^","^(printc b)^")"
    | Get (a,b) -> "(*(("^c_value^" *)(getBinding("^(printc a)^","^(printc b)^")->value)))" (*TODO fix path search *)
    | MALLOC (a,b,c) -> (printc a)^" "^(printc (Ptr b))^" = malloc("^(printc c)^")"
    | Ptr a -> "*"^(printc a) 
    | Adress a -> "&"^(printc a)
    | ToByte a -> (printc a)^".byte"
    | ToOper (a,b,c) -> (printc b) ^" "^a^" "^(printc c)
    | ToLeft a -> "(*("^ (printc a) ^ ".p.left))"
    | ToRight a -> "(*("^ (printc a) ^ ".p.right))"
    | Sizeof a -> "sizeof("^ (printc a)^")"
    | ToCast (a,b) -> "((" ^ a ^")"^(printc b)^")"


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
      let rec parse_module env pth  = function  
          Longident ident -> let (Environment e) = (lookup_path env (convert_path ident)) in e
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
            let name = (Ident.name id) in
            let comp = (parse_computation env (name::path) functlist term) in
            let data = BVal ((Ident.name id), path, (comp)) in 
              data :: (parse_struct (data :: env) path xs) in
        
    (* top level *)
    match program with Structure strt -> (!functlist,(parse_struct [] [] strt))
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
      | BVal (name, _, comp) :: ls -> let ptr = make_ptr (name::path) in
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
        | FB (pth,var,m) -> let npath = make_ptr (name::pth) in
          fctr_list := (Fctr npath) :: !fctr_list;
          (Share,name,npath)::(select path ls)) in 

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

    (* add ; and indentation *)
    let format n ls = let indent = (String.concat "" (List.map (fun x -> " ") (1 -- n))) in
        (List.map (fun x -> (indent ^ x ^ ";")) ls) in

    (* add associations to bindings *)
    let rec print_assoc binding = function [] -> []
      | (ty,strl,strr)::xs -> let ui = (match ty with Call -> "1" | _ -> "0") in
        let temp = ("insertBinding(&"^binding^", \""^strl^"\" , "^strr^", "^ui^");") in
        temp :: (print_assoc binding xs) in

    let func_end = ("}\n"::[]) in

    (* print_strcts: convert structs into mallocs and bindings *)
    let rec print_strcts = function [] -> ([],[])
      | Strct (pth,assocs) :: xs -> 
        let ptr = (make_ptr pth) in
        let binding = (getmod ptr) in
        let decl = (c_strc ^" * "^ptr^" = malloc(sizeof("^ c_strc ^"));") in
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
        (String.concat "\n" ( (definition::body) @ func_end)) :: (print_lambdas xs) in

    (* print a gettr *)
    let rec print_getters = function [] -> []
      | Gettr  (ptr,comp) :: xs -> let definition = ("LOCAL "^c_value^" "^ptr^"(void){") in
        let setup = c_binding^" "^const_env^" = NULL" in
        let body = (format 1 (setup :: (print_computation comp))) in
        (String.concat "\n" ( (definition::body) @ func_end ) ) :: (print_getters xs) in

    (* print some local declarations *)
    let rec print_decl = function [] -> [] 
      | Gettr  (ptr,comp) :: xs -> ("LOCAL "^c_value^" "^ptr^"(void);") :: (print_decl xs) in

    (* print fnctrs TODO *)
    let rec print_fctrs = function [] -> []
      | (Fctr name) :: xs -> let definition = c_strc^"* "^name^"("^c_strc^"* "^const_str^"){" in
        let body = (format 1 ["return NULL;"]) in 
        (String.concat "\n" ( (definition::body) @ func_end)) :: (print_fctrs xs) in

    (* header and footer for the compiled result *)
    let header = ["// Compiled by lanren"; "#include \"miniml.h\"";  ""] in
    let separate name = function [] -> []
      | x::xs as ls -> ["//---------------"^ name ^ "----------------------"; ""] @ ls @ [""] in
    let footer = ["";"// Include the entrypoints & binding code"; "#include \"binding.c\""; "#include \"data.c\"" ; 
            "#include \"entry.c\""; ""] in

    (* Top Level *)
    let (lambda_list,omega) = omega_transformation program in
    let (gettr_lst,strct_list,fctr_list,top_level) = theta_transformation omega in 
    let dec_ls = (separate "declarations" (print_decl gettr_lst))
    and pl_ls =  (separate "Closures" (print_lambdas (List.rev lambda_list)))
    and pv_ls =  (separate "Values" (print_getters (List.rev gettr_lst)))
    and fc_ls = (separate "Functors" (print_fctrs (List.rev fctr_list)))
    and pb_ls = (separate "Boot" (boot_up (strct_list) top_level )) in
    ((String.concat "\n"  (header @ dec_ls @ pl_ls @ pv_ls @ fc_ls @ pb_ls @ footer)) ^ "\n")
  
end


