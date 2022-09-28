;; Inspired of
;; http://sixty-north.com/blog/writing-the-simplest-emacs-company-mode-backend
;; http://sixty-north.com/blog/a-more-full-featured-company-mode-backend.html

(require 'cl-lib)
(require 'company)

(defconst liquidsoap-completions
  '("source.blank" "sine" "playlist"))

(defun company-liquidsoap-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-liquidsoap-backend))
    (prefix (and (eq major-mode 'liquidsoap-mode) (company-grab-symbol)))
    (candidates
    (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      liquidsoap-completions))))

(add-to-list 'company-backends 'company-liquidsoap-backend)

(provide 'liquidsoap-completion)
