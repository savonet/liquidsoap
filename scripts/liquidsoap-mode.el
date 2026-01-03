;; liquidsoap-mode.el -- Liquidsoap major mode
;; Copyright (C) 2003-2026 Samuel Mimram

(require 'liquidsoap-completion)

(defvar liquidsoap-font-lock-keywords
 '(
   ("#.*" . 'font-lock-comment-face)
   ("^\\(%ifdef .*\\|%ifndef .*\\|%ifencoder .*\\|%ifnencoder .*\\|%ifversion .*\\|%else .*\\|%endif\\|%include\\)" . 'font-lock-preprocessor-face)
   ("\\<\\(fun\\|def\\|rec\\|replaces\\|eval\\|begin\\|end\\|if\\|then\\|else\\|elsif\\|let\\|try\\|catch\\|while\\|for\\|in\\|to\\|do\\|open\\)\\>\\|->\\|;" . font-lock-keyword-face)
   ("\\<\\(and\\|or\\|not\\|mod\\|??\\)\\>\\|:=" . font-lock-builtin-face)
   ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)
   ("\\<def[ \t]+\\([^ (]*\\)" 1 'font-lock-function-name-face)
  )
)

(defvar liquidsoap-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Allow some extra characters in words
    (modify-syntax-entry ?_ "w" st)
    ;; Comments
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for Liquidsoap major mode.")

(defvar liquidsoap-tab-width 2)

;see http://www.emacswiki.org/emacs/ModeTutorial
(defun liquidsoap-indent-line ()
  "Indent current Liquidsoap line"
  (interactive)
  (beginning-of-line)
  ; At beginning, no indentation
  (if (bobp) (indent-line-to 0)
    ; not-indented is a boolean saying we found a match looking backward
    ; cur-indent is the current indetation
    (let ((not-indented t) cur-indent)
      ; De-indent after end
      (if (looking-at "^[ \t]*\\(end\\|else\\|elsif\\|then\\|%endif\\|try\\|catch\\)")
        (progn
          (save-excursion
            (forward-line -1)
            (setq cur-indent (- (current-indentation) liquidsoap-tab-width)))
          (if (< cur-indent 0) (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            ; Indent as much as the last end
            (if (looking-at "^[ \t]*\\(end\\|%endif\\)")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              ; Increment if we find that we are in a block
              (if (looking-at "^[ \t]*\\(def\\|if\\|then\\|else\\|elsif\\|%ifdef\\|.*=$\\|try\\|catch\\|for\\|while\\)")
                  (progn
                    (setq cur-indent (+ (current-indentation) liquidsoap-tab-width))
                    (setq not-indented nil))
                ; Same as previous line otherwise
                (if (bobp) (setq not-indented nil))
              )
            )
          )
        )
      )
      ; If we didn't see an indentation hint, then allow no indentation
      (if cur-indent (indent-line-to cur-indent) (indent-line-to 0))
    )
  )
)

(define-derived-mode liquidsoap-mode fundamental-mode
  "Liquidsoap" "Major mode for Liquidsoap files."
  :syntax-table liquidsoap-mode-syntax-table
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'indent-line-function) 'liquidsoap-indent-line)
  (set (make-local-variable 'font-lock-defaults) '(liquidsoap-font-lock-keywords))
  (setq mode-name "Liquidsoap")
)

(add-hook 'liquidsoap-mode-hook 'company-mode)
(add-hook 'liquidsoap-mode-hook 'init-liquidsoap-completion)

(provide 'liquidsoap-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.liq\\'" . liquidsoap-mode))
