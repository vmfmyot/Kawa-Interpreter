open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | VTab  of tab
  | VString of string
  | VChar of char
  | Null
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}
and tab = {
  type_t: typ;
  fields: (int, value) Hashtbl.t;
}

exception Error of string 
exception Return of value

let typ_of_value = function
| VInt _ -> TInt
| VBool _ -> TBool
| VObj o -> TClass o.cls
| VTab t -> TTab t.type_t
| VString _ -> TString
| VChar _ -> TChar
| Null -> TVoid

(*Fonction auxiliaire pour l'extension sur l'égalité structurelle*)
let rec neqstruct ev1 ev2 =
  match ev1, ev2 with
  | VInt _, VBool _ | VBool _, VInt _ | VInt _, VString _ | VString _, VInt _ 
  | VBool _, VString _ | VString _, VBool _ | VInt _, VObj _ | VObj _, VInt _
  | VString _, VObj _ | VObj _, VString _ | VBool _, VObj _ -> VBool(true)
  | VInt i1, VInt i2 -> VBool(i1 <> i2)
  | VBool b1, VBool b2 -> VBool(b1 <> b2)
  | VString s1, VString s2 -> VBool(s1 <> s2)
  | VChar c1, VChar c2 -> VBool(c1 <> c2)
  | VObj o1, VObj o2 -> (
      if o1.cls <> o2.cls || Hashtbl.length o1.fields <> Hashtbl.length o2.fields then VBool(true)
      else let res = ref true in
        Hashtbl.iter (fun att_name att_val ->
                      if !res && Hashtbl.mem o2.fields att_name && att_name <> "this" then
                          begin
                          let att_val2 = Hashtbl.find o2.fields att_name in
                          if neqstruct att_val att_val2 = VBool(false) then res := false;
                          end
                      else res := false;
                      ) o1.fields;
        VBool(!res) )
  | _, _ -> VBool(false)

let exec_prog (p: program): unit =
  (*Création de l'environnement global du programme.
    On y ajoute toutes les variables globales, et on leur associe la valeur NULL.*)
  let env = Hashtbl.create 16 in
  List.iter (fun (var, _, _) -> Hashtbl.add env var Null) p.globals;
  
  (*On ajoute les classes définies dans notre programme à l'environnement global.*)
  List.iter (fun classe -> 
    let flds = Hashtbl.create (List.length classe.attributes) in
    let obj = VObj {cls = classe.class_name; fields = flds} in
    match classe.parent with
    (*Pour chaque classe du programme :
      si la classe n'a pas de classe parente, on ajoute son nom comme clé dans la Hashtable de
      l'environnement globale, et on lui associe une Hashtable contenant la liste de ses attributs 
      et les mots cles 'this' et 'cl_parent' pour retrouver facilement la classe ou on est et son parent.*)
    | None ->
      (List.iter (fun (s, _, _) -> Hashtbl.add flds s Null) classe.attributes;
          Hashtbl.replace flds "cl_parent" Null;
          Hashtbl.replace flds "this" obj;
          Hashtbl.add env classe.class_name obj
      )
    (*Si la classe en étend une autre, on cherche sa classe parente dans l'environnement global
      puis on fait comme au dessus, mais on ajoute également la liste des attributs de la classe mère.*)
    | Some par ->
      (try 
        (match Hashtbl.find env par with
        | VObj({cls = c_par; fields = flds_par}) ->
              (Hashtbl.iter (fun s _ -> Hashtbl.add flds s Null) flds_par;
              List.iter (fun (s, _, _) -> Hashtbl.add flds s Null) classe.attributes;
              Hashtbl.replace flds "cl_parent" (VObj({cls = c_par; fields = flds_par}));
              Hashtbl.replace flds "this" obj;
              Hashtbl.add env classe.class_name obj
              )
        | _ -> ())
      with Not_found -> raise (Error "Classe parente inexistante"))
    ) p.classes;
    (*On ajoute un champ "return" à la table globale, qui nous permettra de stocker temporairement
      les résultats des méthodes avant de les utiliser. La valeur associée au champs "return" est
      remplacée à chaque nouvel appel de return, dans n'importe quelle méthode.*)
    Hashtbl.add env "return" Null;
  

  (*Evaluation d'un appel de méthode :
    f : le nom de la méthode
    this : l'objet courant sur lequel est appelé la méthode
    args : la liste des arguments de la méthode*)
  let rec eval_call f this args =
    (*On récupère les valeurs associé à l'objet courant (le nom de classe associé et sa Hashtable d'attributs).*)
    let {cls = cl_name; fields = flds} = this in
    try 
      ignore(Hashtbl.find env cl_name);
      (*On récupère la dernière classe qui correspond au nom récupéré dans les classes du programme.*)
      let f_cl = List.find (fun classe -> classe.class_name = cl_name) p.classes in
      List.iter (fun (x, _, _) -> try ignore (Hashtbl.find flds x) with Not_found -> Hashtbl.add flds x Null) f_cl.attributes;
      (*On récupère la dernière méthode qui correspond à celle qu'on veut appeler dans les méthodes de la classe.*)
      let meth_l = List.filter (fun meth -> meth.method_name = f && (List.length meth.params) = (List.length args)) f_cl.methods in
      match args with
      (*Si la liste d'arguments est vide, on exécute la séquence du code.*)
      | [] -> (try let method_def_function = List.find (fun x -> x.params = []) meth_l in
                  exec_seq (method_def_function.code) flds 
              with Not_found ->  raise (Error "Mauvais argument de fonction"))
      (*S'il y a une liste d'arguments non-vide, on évalue les types des arguments donnés pour vérifier qu'ils correspondent bien
        à ceux dans la définition de méthode, on met à jour le mot cles 'this' puis on exécute la séquence du code avec les paramètres récupérés.*)
      | _ ->  (try 
      let method_def_function = 
        List.find (fun m -> try List.iter2 (fun (var, typF) arg -> let eval_typ = typ_of_value arg in 
                                  if (typ_to_string eval_typ) <> (typ_to_string typF) then raise (Error( "Mauvais argument de fonction"))
                                  else Hashtbl.replace flds var arg) m.params args; true with Error _ -> false) meth_l in
                    Hashtbl.replace flds "this" (VObj{cls = cl_name; fields = flds});
                    exec_seq method_def_function.code flds
                                with Not_found -> raise( Error "Mauvais argument de fonction"))
    with Not_found -> raise (Error "Classe inexistante")

  and exec_seq s lenv =
    (*Evaluations de types (int, bool et objet) : assert false en cas d'incohérence*)
    let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false
    and evals s = match eval s with
      | VString s -> s
      | _ -> assert false
        
    (*Evaluation d'une expression : *)  
    and eval (e: expr): value = match e with
      | Int n  -> VInt n
      | Bool b -> VBool b
      | String s -> VString s
      | Char c -> VChar c
      (*Accès mémoire : *)
      | Get(m) ->  (match m with 
          (*Si l'accès est une variable : on la cherche dans l'environnement local, puis dans le global si besoin.*)
          | Var(s) ->
                (try Hashtbl.find lenv s with Not_found -> 
                  try Hashtbl.find env s with Not_found -> 
                    raise (Error "Object doesn't exist"))
          (*Si l'accès mémoire est l'attribut d'un objet :
            - si l'objet n'est pas NULL on cherche le nom de l'attribut qu'on veut récupérer dans la Hashtable des attributs de l'objet
            - si l'objet est NULL on cherche l'attribut qu'on veut récupérer dans l'environnement local.*)
          | Field(f,s) -> (match eval f with
                           | VObj {cls = c; fields = f} -> (try Hashtbl.find f s with Not_found -> raise (Error "Object doesn't exist"))
                           | VTab {type_t = t; fields = f} -> (match s with 
                                                                | "length" -> VInt (Hashtbl.length f) 
                                                                | "tl" -> let i = (Hashtbl.length f) - 1 in let res = Hashtbl.find f i in Hashtbl.remove f i; res
                                                                | "hd" -> Hashtbl.find f 0
                                                                | "copy" -> ( VTab( {type_t = t; fields = f} ) )
                                                                | _ -> raise (Error "Function doesnt exist for tables"))
                           | VString st -> (match s with "length" -> VInt (String.length st) | _ -> raise( Error "Function doesnt exist for strings"))
                           | Null -> (try Hashtbl.find lenv s with Not_found -> try Hashtbl.find env s with Not_found -> raise (Error "Object doesn't exist"))
                           | _ -> raise (Error "Trying to access an object attribute in something other than an object"))
          | TabAccess(s, e) -> (let i = evali e in 
                                try 
                                match Hashtbl.find lenv s with
                                | VTab{type_t = t; fields = f} -> (try Hashtbl.find f i with Not_found -> raise (Error "Out of bounds for table"))
                                | VString s -> VChar (s.[i])
                                | _ -> raise (Error "has to be a table or string")
                                with Not_found ->  raise (Error "Not a table")))

      (*Création d'un nouvel objet :
        - sans constructeur : on crée un nouvel objet de la classe concernée et on initialise sa Hashtable d'attributs
        - avec constructeur : idem, puis on appel la méthode constructor de la classe concernée pour intialiser les attributs avec la
          liste des paramètres*)
      | New(s) -> (match Hashtbl.find env s with VObj{cls = c; fields = flds} -> VObj{cls = s; fields = (Hashtbl.copy flds)} | _ -> raise (Error "Should be an object"))
      | NewCstr(s, l) -> (match Hashtbl.find env s with 
                          | VObj{cls = c; fields = flds} -> let obj = {cls = s; fields = (Hashtbl.copy flds)} in 
                                                            ignore(eval_call "constructor" obj (List.map eval l));
                                                            VObj obj
                          | _ -> raise( Error "Should be an object"))
      (*Divers opérateurs unaires et binaires :
        on évalue les types des expressions (int ou bool en fonction des opérateurs calculatoires ou de comparaison).*)
      | Unop(u, e) -> (match u with
                      | Opp -> VInt( -(evali e) )
                      | Not -> VBool( not(evalb e) ))
      | Binop(b, e1, e2) -> (match b with
                             Add -> VInt((evali e1) + (evali e2))
                           | Sub -> VInt((evali e1) - (evali e2))
                           | Mul -> VInt((evali e1) * (evali e2))
                           | Div -> VInt((evali e1) / (evali e2))
                           | Rem -> VInt((evali e1) mod (evali e2))
                           | Eq -> VBool((evali e1) = (evali e2))
                           | Neq -> VBool((evali e1) <> (evali e2))
                           | Neqstruct ->
                            (let ev1 = eval e1 in let ev2 = eval e2 in
                            neqstruct ev1 ev2)
                         | Eqstruct ->
                            (let ev1 = eval e1 in let ev2 = eval e2 in
                              if neqstruct ev1 ev2 = VBool(true) then VBool(false)
                              else VBool(true))
                           | Lt -> VBool((evali e1) < (evali e2))
                           | Le -> VBool((evali e1) <= (evali e2))
                           | Gt -> VBool((evali e1) > (evali e2))
                           | Ge -> VBool((evali e1) >= (evali e2))
                           | And -> VBool((evalb e1) && (evalb e2))
                           | Or -> VBool((evalb e1) || (evalb e2))
                           | Concat -> VString("\"" ^ evals(e1) ^ evals(e2) ^ "\""))
      (*Appel de méthode : on évalue l'appel de méthode, le type d'objet de l'expression et chaque argument.
        On récupère le résultat dans la variable "return" de l'environnement global (qu'on remplace par Null) et on le renvoit.*)
      | MethCall(e, i, l) ->  (match eval e with 
                              | VObj o -> eval_call i o (List.map eval l);
                                          let res = Hashtbl.find env "return" in Hashtbl.replace env "return" Null; res
                              | VTab {type_t = t; fields = f} ->
                                (match i with
                                | "rem" -> let ind = List.map evali l in 
                                    if List.length ind = 1 then begin
                                      for i = (List.hd ind) + 1 to (Hashtbl.length f) - 1 do let v = Hashtbl.find f i in Hashtbl.replace f (i-1) v; done; 
                                       Hashtbl.remove f ((Hashtbl.length f)-1);
                                       Null; end
                                    else raise (Error "rem takes one argument")
                                | "mem" -> (let ll = List.map eval l in
                                            match ll with
                                            | v::[] ->
                                                  let res = ref false in
                                                  Hashtbl.iter( fun k va -> if v = va then res := true; ) f;
                                                  VBool(!res)
                                            | _ -> raise (Error "mem only takes 1 argument") )
                                | _ -> raise (Error "function not implemented for tables"))
                              | _ -> raise (Error "can't use functions with types other than tables and objects") )
      | This -> Null
      (*Test de type : on évalue le type de l'expression objet et renvoie le test d'égalité du type donné et le type apparent de l'expression *)
      | Instanceof(e, t) -> (let {cls = c; fields = f} = evalo e in 
                            match t with 
                            | TClass cl -> VBool(c = cl)
                            | _ -> raise (Error "type has to be object"))

      (*Transtypage : on évalue le type de l'expression objet et on verifie que le type donné est bien un objet. On renvoie 
         un nouvel objet avec l nom de la classe du type donné et la Hashtable des attributs de l'expression modifiée pour qu'il corresponde à la 
         Hastable du type donné avec les valeurs de l'expressions *)
      | Transtype(t, e) -> 
        (let {cls = c; fields = flds} = evalo e in 
        match t with
        | TClass c -> (match Hashtbl.find env c with 
             | VObj {cls = t_cl; fields = t_flds} -> 
                      (if (Hashtbl.length flds) < (Hashtbl.length t_flds) then 
                            let n_fields = Hashtbl.copy flds in 
                            Hashtbl.iter (fun a b -> if not (Hashtbl.mem n_fields a) then Hashtbl.add n_fields a b) t_flds;
                      VObj {cls = t_cl; fields = n_fields}
                      else let n_fields = Hashtbl.copy t_flds in
                            Hashtbl.iter (fun a b -> if Hashtbl.mem n_fields a then Hashtbl.replace n_fields a b) flds;
                            VObj {cls = t_cl; fields = n_fields})
             | _ -> raise (Error "Should be an object"))
        | _ ->  raise (Error "type has to be object") )
      (*Super : *)
      | Super(l) -> eval (SuperCstr("constructor", l))
      | SuperCstr(f, l) -> (try 
                          match Hashtbl.find lenv "this" with 
                          | VObj{cls = c; fields = flds} -> 
                            (try
                                match Hashtbl.find lenv "cl_parent" with 
                                  | VObj{cls = c_par; fields = f_par} ->
                                        Hashtbl.replace flds "cl_parent" (Hashtbl.find f_par "cl_parent");
                                        eval_call f ({cls = c_par; fields = flds}) (List.map eval l);
                                        Hashtbl.replace flds "cl_parent" ( VObj{cls = c_par; fields = f_par});
                                        Hashtbl.replace lenv "this" (VObj{cls = c; fields = flds});
                                        let res = Hashtbl.find env "return" in Hashtbl.replace env "return" Null; res
                                  | _ -> raise (Error "Should be an object")
                              with Not_found -> raise (Error "class is an orphan"))
                          | _ -> raise (Error "Should be an object")
                        with Not_found -> raise (Error "trying to call super outside of class definition"))
      | NewTab(t, o) -> (match o with
                        | Some e -> let i = evali e in 
                          VTab {type_t = t; fields = Hashtbl.create i}
                        | None -> VTab {type_t = t; fields = Hashtbl.create 0})
      | TabVal(e) -> let l = List.length e in let f = Hashtbl.create l in let t = ref TVoid in
                    List.iteri (fun i ex -> let ev = eval ex in Hashtbl.add f i ev; t := (typ_of_value ev)) e;
                    VTab {type_t = (!t); fields = f}
  
   in
let eval_glob = 
      List.iter (fun (var, t, o) -> match o with Some e -> (match t with
                                                          | TClass _ -> raise (Error "variable cant be initialized with object")
                                                          | _ -> let ev = eval e in Hashtbl.add env var ev; 
                                                                 if not (Hashtbl.mem lenv var) then Hashtbl.add lenv var ev )
                                                | None -> ()) p.globals
  in 
    (*Exécution d'une instruction : *)
    let rec exec (i: instr): unit = 
      match i with
      | Print e -> (match (eval e) with
                  | VString s -> Printf.printf "%s\n" (if String.contains s '\\' then (String.fold_left (fun acc x -> if x = '\\' then acc
                                                                                      else acc ^ (Char.escaped x)) "" s)
                                                      else s )
                  | VInt n -> Printf.printf "%d\n" n
                  | VBool b -> Printf.printf "%b\n" b
                  | VChar c -> Printf.printf "%c\n" c
                  | _ -> ())
      | If(e, i1, i2) -> if evalb e then exec_seq i1
                         else exec_seq i2
      | While(e, i) -> (while evalb e do exec_seq i done)
      
      (*Boucle for :
        i1 = instruction de départ
        e = condition de boucle
        i2 = incrémentation/décrémentation
        i = séquence d'instructions (contenu de la boucle)
        On vérifie que i1 initialise un compteur (donc qu'il s'agit d'un set avec entier)
        puis on éxecute la séquence d'instruction suivie de l'instruction de boucle (i2)
        tant que la condition est respectée. *)
      | For((s,t,v), e1, e2, i) ->
          ( let ev = eval v in
            try ignore(Hashtbl.find lenv s); raise (Error "Variable exists outside the loop")
            with Not_found ->   
              (Hashtbl.add lenv s ev;
              while (evalb e1) do exec_seq i; Hashtbl.replace lenv s (eval e2); done;
              Hashtbl.remove lenv s) )
      | ForTab((s, _), st, i) ->
          ( try match Hashtbl.find lenv st with
                | VTab {type_t=t;fields=f} -> 
                  ( try ignore(Hashtbl.find lenv s); raise (Error "Variable exists outside the loop")
                    with Not_found ->   
                    ( Hashtbl.add lenv s (Hashtbl.find f 0);
                      let ind = ref 0 in
                      while (!ind) < (Hashtbl.length f)-1 do exec_seq i; incr ind; Hashtbl.replace lenv s (Hashtbl.find f !ind); done;
                      exec_seq i;
                      Hashtbl.remove lenv s ) ) 
                | _ -> ()  
            with Not_found -> raise (Error "Should be a table") )
      (*Set :
        - si on veut assigner une expression à une variable, on évalue l'expression puis on l'ajoute à la variable dans l'environnement global
        - si on veut assigner une expression à un objet, on évalue l'expression, on évalue l'objet :
                - s'il n'est pas NULL on ajoute l'expression à l'attribut correspondant dans la liste des attributs de l'objet
                - s'il est NULL, on suppose que l'expression f correspond à la variable this et on ajoute l'attribut que l'on veut set avec l'expression dans l'environnement local.*)
      | Set(m,e) -> (let val_expr = eval e in match m with
                    | Var(s)-> (Hashtbl.replace lenv s val_expr)
                    | Field(f,s) ->
                      ((match eval f with
                           | VObj {cls = c; fields = f} -> Hashtbl.replace f s val_expr
                           | Null -> Hashtbl.replace lenv s val_expr
                           | _ -> raise (Error "Trying to access an object attribute in something other than an object")))
                    | TabAccess(s, ex) -> 
                      (let i = evali ex in try 
                          match Hashtbl.find lenv s with
                          |  VTab{type_t = t; fields = f} -> Hashtbl.replace f i val_expr
                          | _ -> raise (Error "has to be a table")
                       with Not_found -> raise (Error "table doesnt exist")))
      (*Return : on replace la valeur du champs "return" de la Hashtable globale par l'expression évaluée.*)
      | Return(e) ->  Hashtbl.replace env "return" (eval e)
      | Expr e -> ignore(eval e)
      | Lvar v -> List.iter (fun (id, t, o) -> match o with Some e -> Hashtbl.add lenv id (eval e) | None -> Hashtbl.add lenv id Null) v

    and exec_seq s = 
      eval_glob;
      List.iter exec s
    in
    exec_seq s
  in
  exec_seq p.main (Hashtbl.create 1)
