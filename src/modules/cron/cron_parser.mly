
%start cron
%type <Cron_base.t> cron

%token STAR
%token DASH
%token SLASH
%token COMMA
%token EOF
%token YEARLY
%token MONTHLY
%token WEEKLY
%token DAILY
%token HOURLY
%token <int> INT
%token <int> WEEK_DAY
%token <int> MONTH

%%

week_day_entry:
  | INT      {
      if $1 < 0 || 7 < $1 then raise (Cron_base.Parse_error "Week day should be in the 0-7 range!");
      $1 mod 6
  }
  | WEEK_DAY { $1 }

week_day_list:
  | week_day_entry COMMA week_day_list { $1 :: $3 }
  | week_day_entry                     { [$1] }

week_day:
  | STAR                               { `Any }
  | week_day_entry                     { `Int $1 }
  | week_day_entry COMMA week_day_list { `List ($1::$3) }
  | week_day_entry DASH week_day_entry { `Range ($1, $3) }
  | STAR SLASH week_day_entry          { `Step $3 }

month_entry:
  | INT      {
      if $1 < 1 || 12 < $1 then raise (Cron_base.Parse_error "Month should be in the 1-12 range!");
      $1
  }
  | MONTH { $1 }

month_list:
  | month_entry COMMA month_list { $1 :: $3 }
  | month_entry                  { [$1] }

month:
  | STAR                         { `Any }
  | month_entry                  { `Int $1 }
  | month_entry COMMA month_list { `List ($1::$3) }
  | month_entry DASH month_entry { `Range ($1, $3) }
  | STAR SLASH month_entry       { `Step $3 }

month_day_entry:
  | INT      {
      if $1 < 1 || 31 < $1 then raise (Cron_base.Parse_error "Month day should be in the 1-31 range!");
      $1
  }

month_day_list:
  | month_day_entry COMMA month_day_list { $1 :: $3 }
  | month_day_entry                      { [$1] }

month_day:
  | STAR                                 { `Any }
  | month_day_entry                      { `Int $1 }
  | month_day_entry COMMA month_day_list { `List ($1::$3) }
  | month_day_entry DASH month_day_entry { `Range ($1, $3) }
  | STAR SLASH month_day_entry           { `Step $3 }

hour_entry:
  | INT      {
      if $1 < 0 || 23 < $1 then raise (Cron_base.Parse_error "Hour should be in the 0-23 range!");
      $1
  }

hour_list:
  | hour_entry COMMA hour_list { $1 :: $3 }
  | hour_entry                 { [$1] }

hour:
  | STAR                       { `Any }
  | hour_entry                 { `Int $1 }
  | hour_entry COMMA hour_list { `List ($1::$3) }
  | hour_entry DASH hour_entry { `Range ($1, $3) }
  | STAR SLASH hour_entry      { `Step $3 }

minute_entry:
  | INT      {
      if $1 < 0 || 59 < $1 then raise (Cron_base.Parse_error "Minute should be in the 0-59 range!");
      $1
  }

minute_list:
  | minute_entry COMMA minute_list { $1 :: $3 }
  | minute_entry                   { [$1] }

minute:
  | STAR                           { `Any }
  | minute_entry                   { `Int $1 }
  | minute_entry COMMA minute_list { `List ($1::$3) }
  | minute_entry DASH minute_entry { `Range ($1, $3) }
  | STAR SLASH minute_entry        { `Step $3 }

cron:
  | YEARLY { { Cron_base.minute = `Int 0; hour = `Int 0; month_day = `Int 1; month = `Int 1; week_day = `Any } }
  | MONTHLY { { Cron_base.minute = `Int 0; hour = `Int 0; month_day = `Int 1; month = `Any; week_day = `Any } }
  | WEEKLY { { Cron_base.minute = `Int 0; hour = `Int 0; month_day = `Any; month = `Any; week_day = `Int 0 } }
  | DAILY { { Cron_base.minute = `Int 0; hour = `Int 0; month_day = `Any; month = `Any; week_day = `Any } }
  | HOURLY { { Cron_base.minute = `Int 0; hour = `Any; month_day = `Any; month = `Any; week_day = `Any } }
  | minute hour month_day month week_day EOF {
    { Cron_base.minute = $1; hour = $2; month_day = $3; month = $4; week_day = $5 }
  }
