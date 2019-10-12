(provide 'mod-scala)

;; ideally ensime should not be used as metals does the same work
;; somehow scala-mode depends on ensime and emacs was giving error bec of t his
;; including it for this reason.
(use-package ensime
  :ensure t)

;; (add-to-list 'exec-path "/usr/local/bin")
;; (setq
;;   ensime-sbt-command "/Users/Ovais/scala-practice/fpinscala/sbt"
;;   sbt:program-name "/Users/Ovais/scala-practice/fpinscala/sbt")

;; Instrcutions taken from https://scalameta.org/metals/docs/editors/emacs.html
;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
