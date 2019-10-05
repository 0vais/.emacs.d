;; Read these for learning keybindings for evil mode and some pointers too for
;; emacs in general
;; https://github.com/noctuid/evil-guide
;; https://github.com/noctuid/general.el

(provide 'keybindings)

;; i am using key-binding specific to modes provided by evil-collection.
;; for some global bindings i am using this

(general-create-definer my-leader-def
  :states '(normal insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :prefix-command 'my-leader-prefix-command
  :prefix-map 'my-leader-prefix-map)

;; for cases which are not covered by evil-collections like org-mode.
;; here this local map is defined and in org-mode/or in any corresponding mode
;; the key bindings will be defined.
(general-create-definer my-local-leader-def
  :states '(normal insert emacs)
  :prefix "SPC SPC"
  :non-normal-prefix "M-SPC SPC"
  :prefix-command 'my-local-leader-prefix-command
  :prefix-map 'my-local-leader-prefix-map)

(my-leader-def
  "a" '(ovi/org-tasks-agenda :which-key "Org Tasks Agenda")
  "N" '(ovi/org-notes-agenda :which-key "Org Notes Agenda")
  "/" '(counsel-ag :which-key "ag")
  "TAB" '(ivy-switch-buffer :which-key "Counsel Switch Buffer")
  "b" '(ibuffer :which-key "Open IBuffer")
  "B" '(ibuffer-other-window :which-key "Win Open Buffer")
  ;; "w" '(hydra-window/body :which-key "Window")
  "d" '(counsel-dired-jump :which-key "Dired")
  "f" '(counsel-find-file :which-key "Find file"))

;;; keybindings.el ends here
