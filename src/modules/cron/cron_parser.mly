
%start cron
%type <Cron_base.t> cron

%token INT_0
%token INT_1
%token INT_2
%token INT_3
%token INT_4
%token INT_5
%token INT_6
%token INT_7
%token INT_8
%token INT_9
%token INT_10
%token INT_11
%token INT_12
%token INT_13
%token INT_14
%token INT_15
%token INT_16
%token INT_17
%token INT_18
%token INT_19
%token INT_20
%token INT_21
%token INT_22
%token INT_23
%token INT_24
%token INT_25
%token INT_26
%token INT_27
%token INT_28
%token INT_29
%token INT_30
%token INT_31
%token INT_32
%token INT_33
%token INT_34
%token INT_35
%token INT_36
%token INT_37
%token INT_38
%token INT_39
%token INT_40
%token INT_41
%token INT_42
%token INT_43
%token INT_44
%token INT_45
%token INT_46
%token INT_47
%token INT_48
%token INT_49
%token INT_50
%token INT_51
%token INT_52
%token INT_53
%token INT_54
%token INT_55
%token INT_56
%token INT_57
%token INT_58
%token INT_59
%token STAR
%token DASH
%token SLASH
%token EOF
%token YEARLY
%token MONTHLY
%token WEEKLY
%token DAILY
%token HOURLY
%token <int> WEEK_DAY
%token <int> MONTH

%%

week_day_entry:
  | INT_0 { 0 }
  | INT_1 { 1 }
  | INT_2 { 2 }
  | INT_3 { 3 }
  | INT_4 { 4 }
  | INT_5 { 5 }
  | INT_6 { 6 }
  | INT_7 { 0 }
  | WEEK_DAY { $1 }

week_day:
  | STAR                               { `Any }
  | week_day_entry                     { `Int $1 }
  | week_day_entry DASH week_day_entry { `Range ($1, $3) }
  | STAR SLASH week_day_entry          { `Step $3 }

month_entry:
  | INT_1 { 1 }
  | INT_2 { 2 }
  | INT_3 { 3 }
  | INT_4 { 4 }
  | INT_5 { 5 }
  | INT_6 { 6 }
  | INT_7 { 7 }
  | INT_8 { 8 }
  | INT_9 { 9 }
  | INT_10 { 10 }
  | INT_11 { 11 }
  | INT_12 { 12 }
  | MONTH { $1 }

month:
  | STAR                         { `Any }
  | month_entry                  { `Int $1 }
  | month_entry DASH month_entry { `Range ($1, $3) }
  | STAR SLASH month_entry       { `Step $3 }

month_day_entry:
  | INT_1 { 1 }
  | INT_2 { 2 }
  | INT_3 { 3 }
  | INT_4 { 4 }
  | INT_5 { 5 }
  | INT_6 { 6 }
  | INT_7 { 7 }
  | INT_8 { 8 }
  | INT_9 { 9 }
  | INT_10 { 10 }
  | INT_11 { 11 }
  | INT_12 { 12 }
  | INT_13 { 13 }
  | INT_14 { 14 }
  | INT_15 { 15 }
  | INT_16 { 16 }
  | INT_17 { 17 }
  | INT_18 { 18 }
  | INT_19 { 19 }
  | INT_20 { 20 }
  | INT_21 { 21 }
  | INT_22 { 22 }
  | INT_23 { 23 }
  | INT_24 { 24 }
  | INT_25 { 25 }
  | INT_26 { 26 }
  | INT_27 { 27 }
  | INT_28 { 28 }
  | INT_29 { 29 }
  | INT_30 { 30 }
  | INT_31 { 31 }

month_day:
  | STAR                                 { `Any }
  | month_day_entry                      { `Int $1 }
  | month_day_entry DASH month_day_entry { `Range ($1, $3) }
  | STAR SLASH month_day_entry           { `Step $3 }

hour_entry:
  | INT_0 { 0 }
  | INT_1 { 1 }
  | INT_2 { 2 }
  | INT_3 { 3 }
  | INT_4 { 4 }
  | INT_5 { 5 }
  | INT_6 { 6 }
  | INT_7 { 7 }
  | INT_8 { 8 }
  | INT_9 { 9 }
  | INT_10 { 10 }
  | INT_11 { 11 }
  | INT_12 { 12 }
  | INT_13 { 13 }
  | INT_14 { 14 }
  | INT_15 { 15 }
  | INT_16 { 16 }
  | INT_17 { 17 }
  | INT_18 { 18 }
  | INT_19 { 19 }
  | INT_20 { 20 }
  | INT_21 { 21 }
  | INT_22 { 22 }
  | INT_23 { 23 }

hour:
  | STAR                       { `Any }
  | hour_entry                 { `Int $1 }
  | hour_entry DASH hour_entry { `Range ($1, $3) }
  | STAR SLASH hour_entry      { `Step $3 }

minute_entry:
  | INT_0 { 0 }
  | INT_1 { 1 }
  | INT_2 { 2 }
  | INT_3 { 3 }
  | INT_4 { 4 }
  | INT_5 { 5 }
  | INT_6 { 6 }
  | INT_7 { 7 }
  | INT_8 { 8 }
  | INT_9 { 9 }
  | INT_10 { 10 }
  | INT_11 { 11 }
  | INT_12 { 12 }
  | INT_13 { 13 }
  | INT_14 { 14 }
  | INT_15 { 15 }
  | INT_16 { 16 }
  | INT_17 { 17 }
  | INT_18 { 18 }
  | INT_19 { 19 }
  | INT_20 { 20 }
  | INT_21 { 21 }
  | INT_22 { 22 }
  | INT_23 { 23 }
  | INT_24 { 24 }
  | INT_25 { 25 }
  | INT_26 { 26 }
  | INT_27 { 27 }
  | INT_28 { 28 }
  | INT_29 { 29 }
  | INT_30 { 30 }
  | INT_31 { 31 }
  | INT_32 { 32 }
  | INT_33 { 33 }
  | INT_34 { 34 }
  | INT_35 { 35 }
  | INT_36 { 36 }
  | INT_37 { 37 }
  | INT_38 { 38 }
  | INT_39 { 39 }
  | INT_40 { 40 }
  | INT_41 { 41 }
  | INT_42 { 42 }
  | INT_43 { 43 }
  | INT_44 { 44 }
  | INT_45 { 45 }
  | INT_46 { 46 }
  | INT_47 { 47 }
  | INT_48 { 48 }
  | INT_49 { 49 }
  | INT_50 { 50 }
  | INT_51 { 51 }
  | INT_52 { 52 }
  | INT_53 { 53 }
  | INT_54 { 54 }
  | INT_55 { 55 }
  | INT_56 { 56 }
  | INT_57 { 57 }
  | INT_58 { 58 }
  | INT_59 { 59 }

minute:
  | STAR                           { `Any }
  | minute_entry                   { `Int $1 }
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
