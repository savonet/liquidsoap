;; Inspired of
;; http://sixty-north.com/blog/writing-the-simplest-emacs-company-mode-backend
;; http://sixty-north.com/blog/a-more-full-featured-company-mode-backend.html

(require 'cl-lib)
(require 'company)

(require 'liquidsoap-completions)

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
    (prefix
      (and
        (eq major-mode 'liquidsoap-mode)
        ;; we don't use company-grab-symbol here because we want to match dots
        (company-grab-line "\\(?:^\\| \\)\\([^ ]*\\)" 1)
      )
    )
    (candidates
     (cl-remove-if-not
       (lambda (c) (string-prefix-p arg c))
       liquidsoap-completions))
    (annotation (liquidsoap-annotation arg))
    (meta (liquidsoap-meta arg))
  )
)

(defun init-liquidsoap-completion ()
  (add-to-list 'company-backends 'company-liquidsoap-backend)
)

(provide 'liquidsoap-completion)
