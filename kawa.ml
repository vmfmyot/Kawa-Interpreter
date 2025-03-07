(**
   Kawa : un petit langage à objets inspiré de Java
 *)

(* Types déclarés pour les attributs, pour les variables, et pour les 
   paramètres et résultats des méthodes. *)
type typ =
  | TVoid
  | TInt
  | TBool
  | TTab of typ
  | TClass of string
  | TString
  | TChar

let rec typ_to_string = function 
  | TVoid    -> "void"
  | TInt     -> "int"
  | TBool    -> "bool"
  | TTab t   -> (match t with 
                | TVoid -> "table"
                | _ -> (typ_to_string t) ^ " tab")
  | TClass c -> c
  | TString -> "string"
  | TChar -> "character"

let string_to_ty = function
  | "void" -> TVoid
  | "int" -> TInt
  | "bool" -> TBool
  | c -> TClass c

type unop  = Opp | Not
type binop = Add | Sub | Mul | Div | Rem
           | Lt  | Le  | Gt | Ge | Eq  | Neq | Eqstruct | Neqstruct
           | And | Or | Concat

(* Expressions *)
type expr =
  (* Base arithmétique *)
  | Int    of int
  | Bool   of bool
  | String of string
  | Char   of char
  | Unop   of unop * expr
  | Binop  of binop * expr * expr
  (* Accès à une variable ou un attribut *)
  | Get      of mem_access
  (* Objet courant *)
  | This
  (* Création d'un nouvel objet *)
  | New      of string
  | NewCstr  of string * expr list
  (* Appel de méthode *)
  | MethCall of expr * string * expr list
  | Instanceof of expr * typ
  | Transtype of typ * expr
  | Super of expr list
  | SuperCstr of string * expr list
  | NewTab of typ * expr option
  | TabVal of expr list

(* Accès mémoire : variable ou attribut d'un objet *)
and mem_access =
  | Var   of string
  | Field of expr (* objet *) * string (* nom d'un attribut *)
  | TabAccess of string * expr

(* Instructions *)
type instr =
  (* Affichage d'un entier *)
  | Print  of expr
  (* Écriture dans une variable ou un attribut *)
  | Set    of mem_access * expr
  (* Structures de contrôle usuelles *)
  | If     of expr * seq * seq
  | While  of expr * seq
  (*| For of instr * expr * instr * seq*)
  | For    of (string * typ * expr) * expr * expr * seq
  | ForTab of (string * typ) * string * seq
  (* Fin d'une fonction *)
  | Return of expr
  (* Expression utilisée comme instruction *)
  | Expr   of expr
  | Lvar   of (string * typ * expr option) list

and seq = instr list

(* Définition de méthode 

   Syntaxe : method <type de retour> <nom> (<params>) { ... }

   Le corps de la méthode est similaire au corps d'une fonction. *)
type method_def = {
    method_name: string;
    code: seq;
    params: (string * typ) list;
    return: typ;
  }
      
type def_typ =
  | Meth    of method_def
  | VarAttr of (string * typ * expr option) list
(* Définition de classe 

   Syntaxe : class <nom de la classe> { ... }
        ou : class <nom de la classe> extends <nom de la classe mère> { ... }

   On considère que toute classe C contient une définition de méthode de nom
   "constructor" et de type de retour void, qui initialise les champs du 
   paramètre implicite this. *)
type class_def = {
    class_name: string;
    attributes: (string * typ * expr option) list;
    methods: method_def list;
    parent: string option;
  }

(* Programme complet : variables globales, classes, et une séquence 
   d'instructions *)
type program = {
    classes: class_def list;
    globals: (string * typ * expr option) list;
    main: seq;
  }
