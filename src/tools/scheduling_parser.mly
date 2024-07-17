
%{
  open Scheduling_defs
%}

%token <int> INT
%token EOF
%token SEC MIN HOUR DAY WDAY
%token LPAR RPAR
%token AND OR
%token INTER
%token LEFT RIGHT
%left AND OR
%start schedule
%type <Scheduling_defs.expr> schedule

%%

schedule:
  | expr EOF     { $1 }
  | expr AND expr EOF { And ($1,$3) }
  | expr OR expr EOF  { Or  ($1,$3) }
;
expr:
  | base { $1 }
  | LEFT expr RIGHT { $2 }
  | LEFT expr AND expr RIGHT { And ($2,$4) }
  | LEFT expr OR expr RIGHT  { Or  ($2,$4) }
;
base:
  | INT WDAY      { WeekDay ($1,$1) }
  | INT HOUR      { Hour ($1,$1) }
  | INT MIN       { Minute ($1,$1) }
  | INT SEC       { Second ($1,$1) }
  | base_interval { $1 }
;
base_interval:
  | interval WDAY   { WeekDay $1 }
  | interval HOUR  { Hour $1 }
  | interval MIN   { Minute $1 }
  | interval SEC   { Second $1 }
;
interval:
  | INT INTER INT { ($1,$3) }
;
