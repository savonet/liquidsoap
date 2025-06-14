let source.mux = ()
let source.mux.track = ()

# Replace the audio track of a source.
# @category Source / Track processing
def source.mux.track.audio(~id=null.make(), ~audio, s) =
  source(id=id, source.tracks(s).{audio=audio})
end

# Replace the video track of a source.
# @category Source / Track processing
def source.mux.track.video(~id=null.make(), ~video, s) =
  source(id=id, source.tracks(s).{video=video})
end

# Replace the audio track of a source by the one of another source.
# @category Source / Track processing
# @param ~audio Source whose audio track is to be taken.
def source.mux.audio(~id=null.make(), ~(audio:source), s) =
  source.mux.track.audio(id=id, audio=source.tracks(audio).audio, s)
end

# Replace the video track of a source by the one of another source.
# @category Source / Track processing
# @param ~audio Source whose video track is to be taken.
def source.mux.video(~id=null.make(), ~(video:source), s) =
  source.mux.track.video(id=id, video=source.tracks(video).video, s)
end

# Replace the midi track of a source by the one of another source.
# @category Source / Track processing
# @param ~midi Source whose midi track is to be taken.
def source.mux.midi(~id=null.make(), ~(midi:source), s) =
  source(id=id, source.tracks(s).{midi=source.tracks(midi).midi})
end

let source.drop = ()

# Remove the audio track of a source.
# @category Source / Track processing
def source.drop.audio(~id=null.make(), s) =
  let {audio = _, ...tracks} = source.tracks(s)
  source(id=id, tracks)
end

# Remove the video track of a source.
# @category Source / Track processing
def source.drop.video(~id=null.make(), s) =
  let {video = _, ...tracks} = source.tracks(s)
  source(id=id, tracks)
end

# Remove the midi track of a source.
# @category Source / Track processing
def source.drop.midi(~id=null.make(), s) =
  let {midi = _, ...tracks} = source.tracks(s)
  source(id=id, tracks)
end

# Remove the metadata track of a source.
# @category Source / Track processing
def source.drop.metadata(~id=null.make(), s) =
  let {metadata = _, ...tracks} = source.tracks(s)
  source(id=id, tracks)
end

# Remove the track marks of a source.
# @category Source / Track processing
def source.drop.track_marks(~id=null.make(), s) =
  let {track_marks = _, ...tracks} = source.tracks(s)
  source(id=id, tracks)
end

# Remove the metadata and track marks of a source.
# @category Source / Track processing
def source.drop.metadata_track_marks(~id=null.make(), s) =
  let {metadata = _, track_marks = _, ...tracks} = source.tracks(s)
  source(id=id, tracks)
end

let settings.amplify =
  settings.make.void(
    "Settings for the amplify operator"
  )

let settings.amplify.override =
  settings.make(
    description=
      "Default metadata used to override amplification.",
    "liq_amplify"
  )

# @docof track.audio.amplify
def track.audio.amplify(
  %argsof(track.audio.amplify[!override]),
  ~override=getter({(settings.amplify.override() : string?)}),
  v,
  t
) =
  track.audio.amplify(%argsof(track.audio.amplify), v, t)
end
