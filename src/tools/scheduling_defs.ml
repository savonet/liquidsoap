
type interval = (int*int)

type expr =

  | WeekDay of interval
  | Hour of interval
  | Minute of interval
  | Second of interval

  | And of expr*expr
  | Or of expr*expr

  | Never
