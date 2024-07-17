
(** Protocol plugin using a strider database *)

open Dtools

(*<section strider>*)

(*<info Perform queries on a strider database, check for server liveness, and
 * remembers about that>*)
  
(** List of protocols for which it should be checked that servers are alive *)
let filtered_proto = Conf.get_list ~default:["smb"] "watchalive.protos"

(** Delay in seconds before updating liveness information *)
let delay = float (Conf.get_int ~default:600 "watchalive.delay")

(** Limit the number of results that mysql queries return *)
let limit = (Conf.get_int ~default:69 "strider.limit")
              
let available = Hashtbl.create 100

let ping s =
  let alive = try ignore (Fetch.ls s) ; true with Fetch.Error _ -> false in
    Hashtbl.replace available s (Unix.time (), alive) ;
    alive

let alive s =
  if Hashtbl.mem available s then
    let (t,v) = Hashtbl.find available s in
      if Unix.time () -. t > delay then ping s else v
  else
    ping s

let idx_path = 0
let idx_proto = 1
let idx_root = 2
let dbox = function None -> assert false | Some x -> x

let resolve = ref (fun q r ->
                     Request.log r "Strider is not initialized!" ;
                     [])

(** At init, connect to the database, and set a resolve function that performs
 * the limited query, and the strips out the dead (or unwatched) servers. *)
let init =
  Init.at_start
    (fun () ->
       let db = 
         { Mysql.dbhost =
             (** Next settings are for connecting to strider database. *)
             (try Some (Conf.get_string "strider.host")
              with Conf.Undefined _ -> None) ;
           Mysql.dbname =
             (try Some (Conf.get_string "strider.name")
              with Conf.Undefined _ -> None) ;
           Mysql.dbport =
             (try Some (Conf.get_int "strider.port")
              with Conf.Undefined _ -> None) ;
           Mysql.dbpwd  =
             (try Some (Conf.get_string "strider.password")
              with Conf.Undefined _ -> None) ;
           Mysql.dbuser =
             (try Some (Conf.get_string "strider.user")
              with Conf.Undefined _ -> None) ;
         } 
       in
       let dbd =
         try
           Mysql.connect db
         with
           | Mysql.Error s ->
               Log.log ~label:"strider" 1
                 (Log.f "Initalization of strider failed: %S" s) ;
               raise (Mysql.Error s)
       in
         ignore (Init.at_stop (fun () -> Mysql.disconnect dbd)) ;
         resolve :=
           fun query r ->
             let query =
               Printf.sprintf
                 "SELECT path, proto, root FROM file WHERE %s LIMIT %d"
                 query limit                      
             in
               try
                 Log.log ~label:"strider" 3
                   (Log.f "Querying: %s" query) ;
                 let l = Mysql.exec dbd query
                 in
                 let rec f acc =
                   match Mysql.fetch l with
                     | None -> acc
                     | Some r ->
                         let proto = dbox r.(idx_proto) in
                         let root = dbox r.(idx_root) in
                           if List.mem proto filtered_proto then
                             if alive root then
                               f ((dbox r.(idx_path))::acc)
                             else
                               f acc
                           else
                             f ((dbox r.(idx_path))::acc)
                 in
                   f []
               with
                 | Mysql.Error s ->
                     Request.log r
                       (Printf.sprintf "Mysql error on %S: %S" query s) ;
                     [])

let fetch arg req maxtime =
  Request.push_indicator req (!resolve arg req)

let scrambled arg req maxtime =
  Request.push_indicator req (!resolve (arg ^ " ORDER by rand()") req)
    
let register =
  Request.protocols#register "strider"
    ~doc:(Doc.trivial
            "dynamically resolves a mysql request on strider's database")
    { Request.resolve = fetch ; Request.static = false } ;
  Request.protocols#register "dj"
    ~doc:(Doc.trivial "same as strider, but scrambles the result")
    { Request.resolve = scrambled ; Request.static = false }
