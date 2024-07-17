
module H = Hashtbl

(** A [plug] is something where plug-ins plug.
  We build [plug] on the top of [Doc.item]. *)

class ['a] plug (name:string) (size:int) (doc:string) (insensitive:bool) =
object (self)
  inherit Doc.item size doc

  val plugins : (string,'a) H.t = H.create size
  method register plugin ?doc ?sdoc v =
    let plugin = if insensitive then String.uppercase plugin else plugin in
    let doc = match doc,sdoc with
      | (Some d), _ -> d
      | _, None -> Doc.trivial "(no doc)"
      | _, (Some s) -> Doc.trivial s
    in
      H.add subsections plugin doc ;
      H.add plugins plugin v
  method is_registered a = Hashtbl.mem plugins a
  method get plugin =
    let plugin = if insensitive then String.uppercase plugin else plugin in
      try
	Some (H.find plugins plugin)
      with
	| Not_found -> None

end

(** Every [plug] plugs in [plugs] *)

let plugs = new Doc.item 10 "All the plugs"

let create ?size ?insensitive ?doc plugname =
  let size = match size with None -> 10 | Some x -> x in
  let insensitive = match insensitive with Some true -> true | _ -> false in
  let doc = match doc with None -> "(no doc)" | Some d -> d in
  let plug = new plug plugname size doc insensitive in
    plugs#add_subsection plugname (plug:>Doc.item) ;
    plug

let list () =
  plugs#list_subsections
