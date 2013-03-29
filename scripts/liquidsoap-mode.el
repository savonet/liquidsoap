;;; liquidsoap-mode.el -- Liquidsoap major mode
;; Based on http://emacswiki.org/emacs/GenericMode

(require 'generic-x)

(define-generic-mode
  'liquidsoap-mode ;; name of the mode to create
  '("#") ;; comments
  '("fun" "def" "begin" "end" "if" "then" "else" "elsif" "and" "or" "not" "mod" "ref" "->" ":=" ";") ;; keywords
  '(
    ;("\\(;\\|:=\\|->\\|\\<not\\>\\)" . 'font-lock-builtin-face)
    ("\\(true\\|false\\)" 1 'font-lock-constant-face)
    ("\\(%ifdef .*\\|%endif\\|%include\\)" . 'font-lock-preprocessor-face)
    ("def[ \t]+\\([^ (]*\\)" 1 'font-lock-function-name-face)
    ;("^[ \t]*\\([a-z]+\\)[ \t]*=" 1 'font-lock-variable-name-face)
   )
  '("\\.liq$") ;; file extension
  nil ;; other functions to call
  "A mode for Liquidsoap files." ;; Description
  )