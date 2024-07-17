
%{

  open Types
  open Lang

  let symbols = Hashtbl.create 10

  let bind var sym = 
    Hashtbl.add symbols var sym

  let resolve var = 
    try
      Hashtbl.find symbols var
    with
      | Not_found -> raise (Lang.Unbound var)

  let nolabelize l =
    let nolabel i =
      Printf.sprintf "arg_%d" i
    in
      fst (List.fold_left (fun (l,i) p -> (nolabel i,p)::l, i+1) ([],1) l)

%}

%token ROOT
%token <string> VAR
%token <string> OP
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> LABEL
%token EOF
%token COMMA
%token SET LET GETS IN
%token LPAR RPAR
%token LBRA RBRA SEP
%start scheduler
%type <Types.source> scheduler

%%

scheduler:
  | settings source EOF { $2 }
;

settings:
  | { }
  | settings_base settings { }
;
settings_base:
  | SET VAR GETS STRING      { Dtools.Conf.set_string $2 $4 }
  | SET VAR GETS INT         { Dtools.Conf.set_int $2 $4 }
  | SET VAR GETS FLOAT       { Dtools.Conf.set_float $2 $4 }
  | SET VAR GETS BOOL        { Dtools.Conf.set_bool $2 $4 }
  | SET VAR GETS string_list { Dtools.Conf.set_list $2 $4 }
;
string_list:
  | LBRA RBRA                        { [] }
  | LBRA inner_string_list RBRA      { $2 }
;
inner_string_list:
  | STRING                           { [$1] }
  | STRING SEP inner_string_list     { $1::$3 }
;

source:
  | LET binding IN source            { $4 }
  | OP parameters                    { match operators#get $1 with
					 | Some f -> ((f $2):>source)
					 | None -> assert false }
  | VAR                              { resolve $1 }
;
binding:
  | VAR GETS source                  { bind $1 $3 ; $3#set_name $1 }
;

list:
  | LBRA RBRA                 { [] }
  | LBRA inner_list RBRA      { $2 }
;
inner_list:
  | parameter SEP inner_list  { $1::$3 }
  | parameter                 { [$1] }
;

parameters:
  | LABEL parameter parameters { ($1,$2)::$3 }
  | parameters_nolabel         { nolabelize $1 }
;
parameters_nolabel:
  |                              { [] }
  | parameter parameters_nolabel { $1::$2 }
;
parameter:
  | INT              { Lang.Int $1 }
  | BOOL             { Lang.Bool $1 }
  | FLOAT            { Lang.Float $1 }
  | STRING           { Lang.String $1 }
  | list             { Lang.List $1 }
  | VAR              { Lang.Source (resolve $1) }
  | OP               { Lang.Source (match operators#get $1 with
				      | Some f -> ((f []):>source)
				      | None -> assert false) }
  | LPAR source RPAR { Lang.Source $2 }
  | LPAR parameter COMMA parameter RPAR { Lang.Product ($2,$4) }
;
