# Decode the picture as it is stored:
settings.ffmpeg.autorotate := false

# Or keep it on and decide the cases liquidsoap leaves alone:
ffmpeg.autorotate.on_undecided(
  fun (question) ->
    if
      question.reason == "odd_rotation"
    then
      [("rotate", [("angle", "#{question.rotation ?? 0.}*PI/180")])]
    else
      null
    end
)
