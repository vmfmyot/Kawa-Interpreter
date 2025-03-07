open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))
let type_inexistent ty = 
  error (Printf.sprintf "type %s doesn't exist" ty)

(* s : nom d'une classe
par : type d'une classe qu'on suppose mère
l : liste de classes
Vérifie que s étend une classe de type par, dans une liste de classes l.
- si s n'a pas de parents, il faut que s et par aient le même type (aka la même classe)
- si s étend une autre classe, si par n'est pas mère de s alors on cherche le parent du parent de s récursivement
dans la liste des classes.*)
let rec find_parent s par l = 
  List.iter (fun x -> if x.class_name = s then
    match x.parent with None -> if s <> (typ_to_string par)
                                then type_error (string_to_ty s) par  
                        | Some p -> if p <> (typ_to_string par) then find_parent p par l) l

let rec complete_attributes c l =
  let cl = List.find (fun x -> x.class_name = c) l in
  match cl.parent with 
    None -> cl.attributes
  | Some s -> (complete_attributes s l)@cl.attributes
 
module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =  
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let add_env_var l tenv =
    List.fold_left (fun env (x, t, _) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = ref (add_env_var p.globals Env.empty) in
  
  let rec check e typ tenv = 
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  (*Type d'une expression : *)
  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | String _ -> TString
    | Char _ -> TChar
    (*Opérateur unaire :
      - s'il s'agit de l'opérateur Not on vérifie que l'expression est de type booléen
      - s'il s'agit de l'opérateur Opp on vérifie que l'expression est de type entier*)
    | Unop(u, e) -> (match u with 
                      Not -> check e TBool tenv; TBool
                    | Opp -> check e TInt tenv; TInt)
    (*Opérateur binaire :
      - s'il s'agit d'un opérateur de comparaison on vérifie que l'expression est de type booléen
      - s'il s'agit d'un opérateur calculatoire on vérifie que l'expression est de type entier*)
    | Binop(b, e1, e2) -> ( match b with
                              And | Or -> check e1 TBool tenv; check e2 TBool tenv; TBool
                            | Add | Sub | Mul | Div | Rem ->  check e1 TInt tenv; check e2 TInt tenv; TInt
                            | Concat -> check e1 TString tenv; check e2 TString tenv; TString
                            | Eqstruct | Neqstruct -> (* let ty_expr = type_expr e1 tenv in check e2 ty_expr tenv;*) TBool 
                            | _ -> check e1 TInt tenv; check e2 TInt tenv; TBool )
    (*Get : on renvoit le type de l'accès mémoire dans l'environnement des types*)
    | Get m -> type_mem_access m tenv
    (*This : on cherche this dans l'environnement des types*)
    | This -> (try Env.find "this" !tenv 
              with Not_found -> error (Printf.sprintf "access to variable \"this\" prohibited outside of class or method definition"))
    (*Nouvel objet :
      - sans constructeur : si on trouve la classe de l'objet que l'on souhaite créer dans les classes du programme, on renvoit
        le type classe (TClass, la classe)
      - avec constructeur : on cherche le nom de la classe de l'objet dans les classes du programme, puis on cherche le constructeur avec
        les paramètres correspondants dans les méthodes de la classe. Si on le trouve, on check les types de chaque paramètres donnés en
        argument avec ceux du constructeur.*)
    | New s -> (try 
                  ignore (List.find (fun x -> x.class_name = s) p.classes);
                  TClass s
                with Not_found -> type_inexistent s)
    | NewCstr (s, e) -> 
      (try 
        let cl = List.find (fun x -> x.class_name = s) p.classes in
        try 
          let cons = List.find (fun x -> (x.method_name = "constructor") && ((List.length x.params) = (List.length e))) cl.methods in
          List.iter2 (fun ex (p, t) -> check ex t tenv) e cons.params;
          TClass s
        with Not_found -> error (Printf.sprintf "No fitting constructeur found for %s" s);
      with Not_found -> type_inexistent s)

    (*Appel de méthode : on récupère le type (aka la classe) de l'expression (correspondant à "l'objet") 
      sur laquelle on appelle la méthode, puis on vérifie que ça correspond à une des classes du programme. On cherche 
      la méthode qu'on veut appeler dans les méthodes de la classe trouvée et on vérifie les types de chacun des paramètres donnés
      en arguments par rapport aux paramètres requis dans la méthode.*)
    | MethCall (e, s, l) -> 
      ( match type_expr e tenv with
      | TClass c -> (let cl = List.find (fun x -> x.class_name = c) p.classes in 
                      let meth_l = List.filter (fun x -> (x.method_name = s) && ((List.length x.params) = (List.length l))) cl.methods in
                      if meth_l = [] then error (Printf.sprintf "Incoherent method call %s" s) 
                      else
                        try
                          let meth = List.find (fun m -> try List.iter2 (fun ex (p, t) -> check ex t tenv) l m.params; true with Error _ -> false ) meth_l in
                          meth.return 
                        with Not_found -> error (Printf.sprintf "Incoherent method call %s" s) )
      | TTab t -> (match s with
                    | "rem" -> List.iter (fun x -> check x TInt tenv) l; TVoid
                    | "mem" -> List.iter (fun x -> check x TInt tenv) l; TBool
                    | _ -> error (Printf.sprintf "Incoherent method call %s" s))
      | _ -> error (Printf.sprintf "Incoherent method call %s" s))
    
    (*Test de type : on cherche le type de l'expression sur laquelle on fait le test pour vérifier que : 
                      1) elle est un objet
                      2) que le type donné est bien un ascendant du type de l'expression *)
    | Instanceof(e, t) -> (match type_expr e tenv with 
                          | TClass c -> find_parent (typ_to_string t) (TClass c) p.classes; TBool 
                          | _ -> error (Printf.sprintf "expression isn't an object"))

    (*Transtypage : on cherche le type de l'expression sur laquelle on fait le test pour vérifier que : 
                      1) elle est un objet
                      2) que le type donné est bien un relative du type de l'expression *)
    | Transtype(t, e) -> (match type_expr e tenv with 
                          | TClass c -> (try find_parent (typ_to_string t) (TClass c) p.classes; t 
                                        with Error _ -> find_parent c t p.classes; t) 
                          | _ -> error (Printf.sprintf "expression isn't an object"))

    (*Super : Si on appele le mot cles 'super' directement, on veut faire un appel au constructor de la classe mère 
              Sinon, on commence par verifier qu'on fait un appel à ce mot clés dans la définition d'une classe en cherchant 'this' dans
    l'environnment local. On continue par vérifier que la classe a un parent sur lequelle chercher la fonction demandé. Une fois qu'on a 
    passer les tests, on vérifie que la méthode demandé avec existe dans la classe mère en cherchant son nom dans ces methodes et vérifiant que le numero d'arguments est correcte *)
    | Super(l) -> type_expr (SuperCstr("constructor", l)) tenv
    | SuperCstr(f, l) -> 
      (try 
        match Env.find "this" !tenv with 
        | TClass c -> 
            (try 
              let cl = List.find (fun x -> x.class_name = c) p.classes in
              match cl.parent with 
              | Some par -> (try 
                            let parent = List.find (fun x -> x.class_name = par) p.classes in
                            try 
                              let meth = List.find (fun x -> (x.method_name = f) && ((List.length x.params) = (List.length l))) parent.methods in
                              meth.return
                            with Not_found -> error (Printf.sprintf "method doesnt exist in class %s" par)
                          with Not_found -> error (Printf.sprintf "Alleged parent class %s doesn't exist" par))
              | None -> error (Printf.sprintf "class %s doesnt have a parent" c)
            with Not_found -> error (Printf.sprintf "class %s doesnt actually exist" c))
        | _ -> error (Printf.sprintf "should be a class")
      with Not_found -> error (Printf.sprintf "calling super outside of class"))
    | NewTab(t, o) ->
      (match o with 
      | Some e -> check e TInt tenv; TTab t 
      | None -> TTab t)
    | TabVal(e) -> 
      (match e with 
      | [] -> TTab TVoid
      | ex::ll -> let t = type_expr ex tenv in List.iter (fun x -> check x t tenv) ll; TTab t) 

  (*Type d'un accès mémoire :
    - s'il sagit d'une variable : on cherche la variable dans l'environnement des types
    - s'il s'agit de l'attribut d'un objet : on récupère le type de l'objet dans l'environnement des types, puis on le
      cherche dans les classes du programme. Si on le trouve, on cherche l'attribut dans la liste des attributs de la classe
      et on récupère son type.*)
  and type_mem_access m tenv = match m with
      Var(s) -> (try Env.find s !tenv with Not_found -> error (Printf.sprintf "variable %s not found" s))
    | Field(e, s) -> 
        (let ty_ex = type_expr e tenv in 
        match ty_ex with
        | TClass _ -> (let res = List.fold_right (fun (a, b, _) acc -> if a = s then b else acc) (complete_attributes (typ_to_string ty_ex) p.classes) TVoid in
                      if res = TVoid then error (Printf.sprintf "attribut %s not found" s) else res)
        | TTab t -> (match s with
                      | "length" -> TInt 
                      | "tl" -> t
                      | "hd" -> t
                      | "copy" -> TTab t
                      | _ -> error (Printf.sprintf "cant call %s from a table" s))
        | TString -> (match s with
                        | "length" -> TInt
                        |  _ -> error (Printf.sprintf "cant call %s from a string" s))
        | _ -> error (Printf.sprintf "a %s can't call this function" (typ_to_string ty_ex)))
    | TabAccess(s, e) ->
      let ty_ex = type_expr e tenv in 
      if ty_ex = TInt then  try
                              (match Env.find s !tenv with
                              | TTab t -> t 
                              | TString -> TChar
                              | _ -> error (Printf.sprintf "should be a table"))
                            with Not_found ->  error (Printf.sprintf "Couldnt find table %s" s)
      else error (Printf.sprintf "cant access table with something other than an integer")
  in

  (*Check que toutes les valeurs données initalement au variable globales sont cohérentes*)
  let check_globals tenv =
    List.iter (fun (x, t, o) -> match o with 
                                    | None -> ()
                                    | Some e -> (match t with 
                                                | TClass s -> error (Printf.sprintf "variable cant be initialized with object") 
                                                | _ -> check e t tenv )) p.globals
  in
  (*Type d'une instruction :*)
  let rec check_instr i ret tenv = match i with
    | Print e -> ( let t = type_expr e tenv in
                    match t with
                    | TInt | TString | TBool | TChar -> ()
                    | _ ->  error (Printf.sprintf "cant print type %s" (typ_to_string t)))
    (*Set : on récupère le type d'accès mémoire de la variable et le type de l'expression 
      que l'on veut set dans l'environnement des types. Si le type de la variable d'accès mémoire est différent
      du type de l'expression, on vérifie si le type de l'expression correspond à une classe qui étend la classe de l'objet
      (accès mémoire).*)
    | Set(m, e) ->
      let t = type_mem_access m tenv in let te = type_expr e tenv in 
      if t <> (TTab TVoid) then
        if t <> te then find_parent (typ_to_string te) t p.classes 
    | While(e, ii) -> check e TBool tenv; check_seq ii ret tenv
    | If(e, i1, i2) -> check e TBool tenv; check_seq i1 ret tenv; check_seq i2 ret tenv
    | For((s,t,v),e1,e2, i) -> let fenv = ref (Env.add s t !tenv) in check v t fenv; check e1 TBool fenv; check e2 t fenv; check_seq i ret fenv (*ou check_seq ii ret tenv*)
    | ForTab((s,t), st, i) -> let fenv = ref (Env.add s t !tenv) in check (Get(Var(st))) (TTab t) fenv; check_seq i ret fenv 
    | Return(e) | Expr(e) -> check e ret tenv
    | Lvar(v) -> let fenv = ref (add_env_var v !tenv) in List.iter (fun (id, t, o) -> match o with Some e -> check e t fenv | None -> ()) v; tenv := !fenv

  (*Check toutes les instructions d'une séquence s*)
  and check_seq s ret tenv = 
    List.iter (fun i -> check_instr i ret tenv) s

  (*Check les définitions de classes : pour chaque classe on crée un environnement avec les attributs de la classe et un 
  indice 'this' qui permet de savoir dans quelle classe on se trouve pour checker iterativement les définitions de methode dans la classe *)
 and check_class c = 
    let cenv = ref (Env.add "this" (TClass(c.class_name)) (add_env_var c.attributes Env.empty)) in
    List.iter (fun x -> check_mdef x x.return cenv) c.methods

  (*Check les définitions de methode : on vérifie que le type de return de la méthode et le même que celui attendue puis on crée un
     environnement à partir de tenv qui connait les paramètres de la méthode pour apres check le code iterativement avec*)
  and check_mdef m ret tenv = 
    if ret <> m.return then type_error ret m.return else
      let menv = ref (add_env m.params !tenv) in
      check_seq m.code ret menv
  
  (*Check itérativement que toutes les classes sont définies correctement*)
  and check_classes c = 
    List.iter (fun x -> check_class x) c
  
  in
  check_globals tenv; check_classes p.classes; check_seq p.main TVoid tenv
