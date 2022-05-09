(** Interaction with a webpage. *)

open Js_of_ocaml
module Html = Dom_html

let doc = Html.document

let button txt action =
  let button_type = Js.string "button" in
  let b = Html.createInput ~_type:button_type doc in
  b##.value := Js.string txt;
  b##.onclick :=
    Dom_html.handler (fun _ ->
        action ();
        Js._true);
  b

let debug s : unit = Firebug.console##debug (Js.string s)

let run _ =
  debug "Running interaction...";

  let top =
    Js.Opt.get
      (doc##getElementById (Js.string "toplevel"))
      (fun () -> assert false)
  in

  let output = Html.createDiv doc in
  output##.id := Js.string "output";
  output##.style##.whiteSpace := Js.string "pre";
  Dom.appendChild top output;

  let textbox = Html.createTextarea doc in
  textbox##.id := Js.string "input";
  textbox##.cols := 80;
  textbox##.rows := 25;
  (* textbox##value := Js.string "# "; *)
  Dom.appendChild top textbox;
  Dom.appendChild top (Html.createBr doc);
  textbox##focus;
  textbox##select;

  (* Current offset in textbox. *)
  let tb_off = ref 0 in

  (* The function to print something. *)
  let print s =
    let s = Js.to_string textbox##.value ^ s in
    tb_off := String.length s;
    textbox##.value := Js.string s;
    (* Scroll down. *)
    Js.Unsafe.set textbox (Js.string "scrollTop")
      (Js.Unsafe.get textbox (Js.string "scrollHeight"))
  in

  (* The function to read from commandline. *)
  let read () =
    let s = Js.to_string textbox##.value in
    let cmd = String.sub s !tb_off (String.length s - !tb_off) in
    tb_off := String.length s;
    cmd
  in

  (* TODO: fill me in *)
  let loop s = print ("Just read: " ^ s ^ "!\n") in

  let b =
    button "Send" (fun () ->
        let s = read () in
        let s =
          let s = ref s in
          let remove_last () =
            if !s = "" then false
            else (
              let c = !s.[String.length !s - 1] in
              c = '\n' || c = '\r')
          in
          while remove_last () do
            (* remove trailing \n *)
            s := String.sub !s 0 (String.length !s - 1)
          done;
          !s
        in
        loop s;
        textbox##focus;
        doc##.documentElement##.scrollTop := doc##.body##.scrollHeight)
  in
  b##.id := Js.string "send";
  Dom.appendChild top b;

  ignore (Js.Unsafe.eval_string "init();");

  Js._false

let () = Html.window##.onload := Html.handler run
