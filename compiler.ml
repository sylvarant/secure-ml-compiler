(*
 * =====================================================================================
 *
 *       Filename:  compiler.ml
 *
 *    Description:  Compile the AST into C?
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 *)

open Mini
open Modules (*opening mini should open this *)


(*-----------------------------------------------------------------------------
 *  The compiler for MiniML ml
 *-----------------------------------------------------------------------------*)
module CCompiler =
struct
    open MiniML 
    open MiniMLMod


    (* Exceptions *) 
    exception Cannot_compile of string

    (* types used during omega translation *)
    type cpath = string list
    and modbindtype = FB of cpath * string * mod_term | SB of cpath * strctbinding list
    and strctbinding = BVal of string * computation | BMod of string * modbindtype 
    and computation = TODO
    
    (* types used during theta translation *)
    type compred = Gettr of string * computation | Strct of cpath * assoc list
    and assoc = action * string * string
    and action = Call | Share
 

    (* convert a path into a list of strings *)
    let rec convert_path = (function Pident id -> [Ident.name id]
        | Pdot (p,str) -> str :: (convert_path p)) 
    
    (* convert a list into a pointer *)
    let make_ptr lst = (String.concat "_" (List.rev lst)) 


   (* 
    * ===  FUNCTION  ======================================================================
    *         Name:    parse_computation
    *  Description:    compiles the lambda calculus
    * =====================================================================================
    *)
    let rec parse_computation env = function _ -> TODO 

       
   (* 
    * ===  FUNCTION  ======================================================================
    *         Name:    omega_transformation
    *  Description:    converts the toplevel into an omega binding
    * =====================================================================================
    *)
    let omega_transformation program = 

        (* look up x in the env *)
        let get_binding x env = 
            let find_binding = (function
                | BVal (nn,_) when (nn = x) -> true
                | BMod (nn,_) when (nn = x) -> true
                | _ -> false) in
            try (List.find (find_binding) env)
            with _ -> raise (Cannot_compile "Identifier not found") in

        (* convert a sequence of structure definitions *)
        let rec parse_struct env path strctls   = 

            (* convert a module definition into a new environment *)
            let rec parse_module env pth modterm = 

                let rec lookup_path env = (function x::[] -> let BMod (_,e) = (get_binding x env) in e
                    | x::xs -> let SB (_,nenv) =  (lookup_path env xs) in
                        let BMod (_,e) = (get_binding x nenv) in e) in
            
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
    *         Name:    theta_transformation
    *  Description:    converts the binding into lists of gettr's struct ptr's and fctr's 
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
                | FB (pth,var,m) -> (Share,name,(make_ptr pth))::(select path ls)) in (* TODO functor's need special treatment *)

        (* top level *)
        let top_level = (select [] binding) in 
        (!gettr_lst,!strct_list,!fctr_list,top_level)
         

   (* 
    * ===  FUNCTION  ======================================================================
    *         Name:    compile
    *  Description:    converts the toplevel into a giant string
    * =====================================================================================
    *)
    let compile program =

        (* add associations to bindings *)
        let rec print_assoc binding = function [] -> []
            | (_,strl,strr)::xs -> let temp = ("insertBinding("^binding^", "^strl^" , "^strr^");") in
                temp :: (print_assoc binding xs) in

        (* print_strcts: convert structs into mallocs and bindings *)
        let rec print_strcts = function [] -> []
            | Strct (pth,assocs) :: xs -> 
                let ptr = (make_ptr pth) in
                let binding = (ptr^"->mod") in
                let decl = [("struct Structure * "^ptr^" = malloc(sizeof(struct Structure));")] in
                let parent = try let tail = (List.tl pth) in match tail with
                    | [] -> []
                    | x::xs -> [(binding^" = "^(make_ptr tail)^".mod;")] 
                    with _ -> [] in
                decl @ parent @ (print_assoc binding assocs) @ (print_strcts xs) in

        (* build the bootup function: where we set it all up *)
        let boot_up strls top =
            let def = "void bootup(void) {"  in
            let body = (print_strcts strls) in
            let body2 = (print_assoc "toplevel"  top) in
                (def :: (body @ body2) @ ["}"]) in

        let rec print_computation comp = ["TODO"] in

        let rec print_getters = function [] -> []
            | Gettr  (ptr,comp) :: xs -> let definition = ("VALUE "^ptr^"(BINDING * mod){") in
                let body = (String.concat "\n" (print_computation comp)) in
                (String.concat "\n" (definition::body::"}\n"::[]) ) :: (print_getters xs) in



        (* header for the compiled result *)
        let header = ["// Compiled by lanren"; "#include \"miniml.h"; ""] in

        let omega = omega_transformation program in
        let (gettr_lst,strct_list,fctr_list,top_level) = theta_transformation omega in 
        ((String.concat "\n"  (header @ (print_getters (List.rev gettr_lst)) @ (boot_up (List.rev strct_list) top_level ))) ^ "\n")
    
end


