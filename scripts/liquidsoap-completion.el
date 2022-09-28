;; Inspired of
;; http://sixty-north.com/blog/writing-the-simplest-emacs-company-mode-backend
;; http://sixty-north.com/blog/a-more-full-featured-company-mode-backend.html

(require 'cl-lib)
(require 'company)

(defconst liquidsoap-completions
  '(
    #("source.metadata" 0 1 (:type "(string) -> [string]" :description "Show metadata."))
    #("source.duration" 0 1 (:type "(string) -> float" :description "Duration of the source."))
    #("playlist" 0 1 (:type "(string) -> source" :description "Stream a playlist."))
    #("ffmpeg.filter.showinfo" 0 1 (:type "(?checksum : bool?, ffmpeg.filter.graph, ffmpeg.filter.video) ->
ffmpeg.filter.video" :description "Ffmpeg filter: Show textual information for each video frame."))
#("ffmpeg.filter.showinfo.create" 0 1 (:type "(?checksum : bool?, ffmpeg.filter.graph) -> unit" :description "Ffmpeg filter: Show textual information for each video frame.. Use this operator to initiate the filt
er independently of its inputs, to be able to send commands to the filter instance."))
#("ffmpeg.filter.showpalette" 0 1 (:type "(?s : int?, ffmpeg.filter.graph, ffmpeg.filter.video) -> ffmpeg.filter.video" :description "Ffmpeg filter: Display frame palette."))
#("ffmpeg.filter.showpalette.create" 0 1 (:type "(?s : int?, ffmpeg.filter.graph) -> unit" :description "Ffmpeg filter: Display frame palette.. Use this operator to initiate the filter independently of its input
s, to be able to send commands to the filter instance."))
#("ffmpeg.filter.showspatial" 0 1 (:type "(?size : string?, ?s : string?, ?win_size : int?, ?win_func : int?,
 ?overlap : float?, ffmpeg.filter.graph, ffmpeg.filter.audio) ->
ffmpeg.filter.video" :description "Ffmpeg filter: Convert input audio to a spatial video output."))
#("ffmpeg.filter.showspatial.create" 0 1 (:type "(?size : string?, ?s : string?, ?win_size : int?, ?win_func : int?,
 ?overlap : float?, ffmpeg.filter.graph) -> unit" :description "Ffmpeg filter: Convert input audio to a spatial video output.. Use this operator to initiate the filter independently of its inputs, to be able to 
send commands to the filter instance."))
#("ffmpeg.filter.showspectrum" 0 1 (:type "(?size : string?, ?s : string?, ?slide : int?, ?mode : int?, ?color : int?,
 ?scale : int?, ?fscale : int?, ?saturation : float?, ?win_func : int?,
 ?orientation : int?, ?overlap : float?, ?gain : float?, ?data : int?,
 ?rotation : float?, ?start : int?, ?stop : int?, ?fps : string?,
 ?legend : bool?, ?drange : float?, ?limit : float?, ?opacity : float?,
 ffmpeg.filter.graph, ffmpeg.filter.audio) -> ffmpeg.filter.video" :description "Ffmpeg filter: Convert input audio to a spectrum video output."))
#("ffmpeg.filter.showspectrum.create" 0 1 (:type "(?size : string?, ?s : string?, ?slide : int?, ?mode : int?, ?color : int?,
 ?scale : int?, ?fscale : int?, ?saturation : float?, ?win_func : int?,
 ?orientation : int?, ?overlap : float?, ?gain : float?, ?data : int?,
 ?rotation : float?, ?start : int?, ?stop : int?, ?fps : string?,
 ?legend : bool?, ?drange : float?, ?limit : float?, ?opacity : float?,
 ffmpeg.filter.graph) -> unit" :description "Ffmpeg filter: Convert input audio to a spectrum video output.. Use this operator to initiate the filter independently of its inputs, to be able to send commands to t
he filter instance."))
#("ffmpeg.filter.showspectrumpic" 0 1 (:type "(?size : string?, ?s : string?, ?mode : int?, ?color : int?, ?scale : int?,
 ?fscale : int?, ?saturation : float?, ?win_func : int?, ?orientation : int?,
 ?gain : float?, ?legend : bool?, ?rotation : float?, ?start : int?,
 ?stop : int?, ?drange : float?, ?limit : float?, ?opacity : float?,
 ffmpeg.filter.graph, ffmpeg.filter.audio) -> ffmpeg.filter.video" :description "Ffmpeg filter: Convert input audio to a spectrum video output single picture."))
#("ffmpeg.filter.showspectrumpic.create" 0 1 (:type "(?size : string?, ?s : string?, ?mode : int?, ?color : int?, ?scale : int?,
 ?fscale : int?, ?saturation : float?, ?win_func : int?, ?orientation : int?,
 ?gain : float?, ?legend : bool?, ?rotation : float?, ?start : int?,
 ?stop : int?, ?drange : float?, ?limit : float?, ?opacity : float?,
 ffmpeg.filter.graph) -> unit" :description "Ffmpeg filter: Convert input audio to a spectrum video output single picture.. Use this operator to initiate the filter independently of its inputs, to be able to sen
d commands to the filter instance."))
    )
)

(defun liquidsoap-annotation (s)
  (format " : %s" (get-text-property 0 :type s))
)

(defun liquidsoap-meta (s)
  (get-text-property 0 :description s)
)

(defun company-liquidsoap-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-liquidsoap-backend))
    (prefix (and (eq major-mode 'liquidsoap-mode) (company-grab-symbol)))
    (candidates
     (cl-remove-if-not
       (lambda (c) (string-prefix-p arg c))
       liquidsoap-completions))
    (annotation (liquidsoap-annotation arg))
    (meta (liquidsoap-meta arg))
  )
)

(add-to-list 'company-backends 'company-liquidsoap-backend)

(provide 'liquidsoap-completion)
