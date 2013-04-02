;;; liquidsoap-mode.el -- Liquidsoap major mode

(defvar liquidsoap-font-lock-keywords
 '(
   ("#.*" . 'font-lock-comment-face)
   ("^\\(%ifdef .*\\|%endif\\|%include\\)" . 'font-lock-preprocessor-face)
   ("\\<\\(fun\\|def\\|begin\\|end\\|if\\|then\\|else\\|elsif\\)\\>\\|->\\|;" . font-lock-keyword-face)
   ("\\<\\(and\\|or\\|not\\|mod\\|ref\\)\\>\\|:=" . font-lock-builtin-face)
   ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)
   ("\\<def[ \t]+\\([^ (]*\\)" 1 'font-lock-function-name-face)
  )
)

(define-derived-mode liquidsoap-mode fundamental-mode
  "Liquidsoap" "Major mode for Liquidsoap files."
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults) '(liquidsoap-font-lock-keywords))
  (setq mode-name "Liquidsoap")
)

(provide 'liquidsoap-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.liq\\'" . liquidsoap-mode))
