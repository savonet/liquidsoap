exception Malformed

type t = {
  value : string;
  scheme : string;
  authority : string option;
  userinfo : string option;
  host : string option;
  port : int option;
  path : string;
  query : string option;
  fragment : string option;
}

val create : string -> t
