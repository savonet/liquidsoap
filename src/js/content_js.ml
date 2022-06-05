open Liquidsoap_lang.Content

type audio_params = { channel_layout : [ `Mono | `Stereo | `Five_point_one ] }
type video_params = { width : int option; height : int option }
type midi_params = { channels : int }

module AudioSpecs = struct
  type kind = [ `Pcm ]
  type params = audio_params
  type data = unit

  let internal_content_type = Some `Audio
  let string_of_kind = function `Pcm -> "pcm"

  let string_of_params { channel_layout } =
    match channel_layout with
      | `Mono -> "mono"
      | `Stereo -> "stereo"
      | `Five_point_one -> "5.1"

  let merge p p' =
    assert (p.channel_layout = p'.channel_layout);
    p

  let compatible p p' = p.channel_layout = p'.channel_layout

  let parse_param label value =
    match (label, value) with
      | "", "mono" -> Some { channel_layout = `Mono }
      | "", "stereo" -> Some { channel_layout = `Stereo }
      | "", "5.1" -> Some { channel_layout = `Five_point_one }
      | _ -> None

  let params () = { channel_layout = `Stereo }
  let kind = `Pcm
  let default_params _ = { channel_layout = `Stereo }
  let kind_of_string = function "audio" | "pcm" -> Some `Pcm | _ -> None
  let is_empty _ = true
  let make ~size:_ _ = ()
  let clear _ = ()
  let blit _ _ _ _ _ = ()
  let fill _ _ _ _ _ = ()
  let sub _ _ _ = ()
  let copy _ = ()
end

module Audio = struct
  include MkContent (AudioSpecs)
end

module VideoSpecs = struct
  type kind = [ `Canvas ]
  type params = video_params
  type data = ()

  let internal_content_type = Some `Video
  let string_of_kind = function `Canvas -> "canvas"

  let string_of_params { width; height } =
    print_optional
      [
        ("width", Option.map (fun x -> string_of_int x) width);
        ("height", Option.map (fun x -> string_of_int x) height);
      ]

  let parse_param label value =
    match label with
      | "width" -> Some { width = Some (int_of_string value); height = None }
      | "height" -> Some { width = None; height = Some (int_of_string value) }
      | _ -> None

  let merge p p' =
    {
      width = merge_param ~name:"width" (p.width, p'.width);
      height = merge_param ~name:"height" (p.height, p'.height);
    }

  let compatible p p' =
    let compare = function
      | None, None -> true
      | Some _, None | None, Some _ -> true
      | Some x, Some y -> x = y
    in
    compare (p.width, p'.width) && compare (p.height, p'.height)

  let params () = { width = None; height = None }
  let kind = `Canvas
  let default_params _ = { width = None; height = None }
  let kind_of_string = function "canvas" -> Some `Canvas | _ -> None
  let is_empty _ = true
  let make ~size:_ _ = ()
  let clear : data -> unit = fun _ -> ()
  let blit : data -> int -> data -> int -> int -> unit = fun _ _ _ _ _ -> ()
  let fill : data -> int -> data -> int -> int -> unit = fun _ _ _ _ _ -> ()
  let sub _ _ _ = ()
  let copy _ = ()
end

module Video = struct
  include MkContent (VideoSpecs)
end

module MidiSpecs = struct
  include NoneSpecs

  let internal_content_type = Some `Midi
end

module Midi = struct
  include MkContent (MidiSpecs)
end
