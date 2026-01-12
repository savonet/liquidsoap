(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

%token LCUR RCUR LBRA RBRA
%token COLON
%token COMMA
%token <string> IDENTIFIER
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NULL
%token EOF

%start json
%type <Json_base.t> json

%type <Json_base.t> json_value
%type <string * Json_base.t> json_object_entry
%type <(string * Json_base.t) list> json_object
%type <Json_base.t list> json_array

%start json5
%type <Json_base.t> json5

%type <Json_base.t> json5_value
%type <string * Json_base.t> json5_object_entry
%type <(string * Json_base.t) list> json5_object
%type <Json_base.t list> json5_array

%%

json:
  | json_value EOF        { $1 }

json5:
  | json5_value EOF       { $1 }

json_value:
  | NULL                  { `Null }
  | STRING                { `String $1 }
  | INT                   { `Int $1 }
  | FLOAT                 { `Float $1 }
  | BOOL                  { `Bool $1 }
  | LBRA RBRA             { `Tuple [] }
  | LBRA json_array RBRA  { `Tuple $2 }
  | LCUR RCUR             { `Assoc [] }
  | LCUR json_object RCUR { `Assoc $2 }

json_array:
  | json_value                  { [$1] }
  | json_value COMMA json_array { $1::$3 }

json_object_entry:
  | STRING COLON json_value  { ($1, $3) }

json_object:
  | json_object_entry                   { [$1] }
  | json_object_entry COMMA json_object { $1::$3 }

json5_value:
  | NULL                   { `Null }
  | STRING                 { `String $1 }
  | INT                    { `Int $1 }
  | FLOAT                  { `Float $1 }
  | BOOL                   { `Bool $1 }
  | LBRA json5_array RBRA  { `Tuple $2 }
  | LCUR json5_object RCUR { `Assoc $2 }

json5_array:
  |                               { [] }
  | json5_value                   { [$1] }
  | json5_value COMMA json5_array { $1::$3 }

json5_object_entry:
  | IDENTIFIER COLON json5_value { ($1, $3) }
  | STRING COLON json5_value     { ($1, $3) }

json5_object:
  |                                       { [] }
  | json5_object_entry                    { [$1] }
  | json5_object_entry COMMA json5_object { $1::$3 }
